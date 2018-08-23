#lang racket/base

(require racket/string
         racket/match
         racket/contract
         (for-syntax racket/base)
         zeromq
         "ipython-message.rkt"
         "ipython.rkt")

(provide call-with-services
         receive-request
         send-response
         make-stream-port
         send-exec-result
         send-status
         (struct-out services))

(define-struct/contract services
  ([heartbeat thread?]
   [shell thread?]
   [control thread?]
   [iopub thread?])
  #:transparent)

(define (call-with-services cfg action)
  (define worker (current-thread))
  (define (serve port port-type thunk)
    (serve-socket/thread cfg (port cfg) port-type worker thunk))
  (define services
    (make-services
     (serve config-hb-port 'rep heartbeat)
     (serve config-shell-port 'router shell)
     (serve config-control-port 'router control)
     (serve config-iopub-port 'pub iopub)))
  (begin0
      (action services)
    (kill-services services)))

(define (receive-request services)
  (match-define (cons msg respond-to) (thread-receive))
  (values msg respond-to))

(define (send-response services respond-to response)
  (thread-send respond-to response))

(define (send-exec-result msg services execution-count data)
  (thread-send (services-iopub services)
               (make-response msg (hasheq 'execution_count execution-count
                                          'data data
                                          'metadata (hasheq))
                              #:msg-type 'execute_result)))

(define/contract (send-status services parent-header status)
  (services? header? (symbols 'idle 'busy) . -> . void?)
  (define iopub (services-iopub services))
  (define header (make-response-header parent-header #:msg-type 'status))
  (define msg (make-message header (hasheq 'execution_state (symbol->string status))))
  (thread-send iopub msg))


(define/contract (make-stream-port services name orig-msg)
  (services? (symbols 'stdout 'stderr) message? . -> . output-port?)
  (define iopub (services-iopub services))
  (define-values (port-name stream-name)
    (case name
      [(stdout) (values "pyout" "stdout")]
      [(stderr) (values "pyerr" "stderr")]))
  (define (send-stream str)
    (thread-send iopub (make-response orig-msg (hasheq 'name stream-name
                                                       'text str)
                                      #:msg-type 'stream)))
  (make-output-port
   port-name
   iopub
   (λ (bstr start end enable-buffer? enable-break?)
     (send-stream (bytes->string/utf-8 (subbytes bstr start end)))
     (- end start))
   void))


;; implements the ipython heartbeat protocol
(define (heartbeat socket _worker)
  (let loop ()
    (define msg (zmq-recv socket))
    (zmq-send socket msg)
    (loop)))

;; implements shell and control protocol
(define (shell-like who socket worker)
  (let loop ()
    (define msg (receive-message! socket))
    (thread-send worker (cons msg (current-thread)))
    (define response (thread-receive))
    (when response (send-message! socket response))
    (loop)))

(define (shell socket worker) (shell-like 'shell socket worker))

(define (control socket worker) (shell-like 'control socket worker))

(define (iopub socket worker)
  (let loop ()
    (define msg (thread-receive))
    (when msg (send-message! socket msg))
    (loop)))

(define (serve-socket endpoint socket-type action)
  (call-with-socket socket-type
    (lambda (socket)
      (zmq-bind socket endpoint)
      (action socket))))

(define (call-with-socket type proc)
  (define socket (zmq-socket type))
  (dynamic-wind
    void
    (lambda () (call-with-continuation-barrier (lambda () (proc socket))))
    (lambda () (zmq-close socket))))

(define (serve-socket/thread cfg port socket-type worker action)
  (define transport (config-transport cfg))
  (define ip (config-ip cfg))
  (define endpoint (format "~a://~a:~a" transport ip port))
  (thread
   (λ () (serve-socket endpoint socket-type
                       (λ (socket) (action socket worker))))))

(define (ipython-serve cfg worker)
  (make-services
   (serve-socket/thread cfg (config-hb-port cfg) 'rep worker heartbeat)
   (serve-socket/thread cfg (config-shell-port cfg) 'router worker shell)
   (serve-socket/thread cfg (config-control-port cfg) 'router worker control)
   (serve-socket/thread cfg (config-iopub-port cfg) 'pub worker iopub)))

(define (kill-services services)
  (kill-thread (services-shell services))
  (kill-thread (services-control services))
  (kill-thread (services-iopub services))
  (kill-thread (services-heartbeat services)))
