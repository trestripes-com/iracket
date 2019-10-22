#lang racket/base
(require (for-syntax racket/base)
         racket/match
         racket/contract
         racket/string
         racket/port
         racket/date
         libuuid
         json
         sha
         zeromq)

;; ============================================================
;; Messages

(provide make-response
         message?
         message-ref)

;; make-response : Message JSExpr [#:msg-type MessageType] -> Message
;; Make a response given a parent header, optionally overriding the
;; message type. If the message type is not given, it is determined (if
;; possible) from the parent message type.
(define (make-response parent content #:msg-type [msg-type #f])
  (define response-header
    (make-response-header (message-header parent) #:msg-type msg-type))
  (make-message response-header content))

;; make-response-header : Header [#:msg-type MessageType] -> Header
;; Make a response header given a parent header, optionally overriding the
;; message type. If the message type is not given, it is determined (if
;; possible) from the parent message type.
(define (make-response-header parent-header #:msg-type [msg-type #f])
  (make-header
   (header-identifiers parent-header)
   parent-header
   (make-hasheq)
   (uuid-generate)
   (header-session-id parent-header)
   (parameterize ((date-display-format 'iso-8601))
     (define now (seconds->date (current-seconds) #f))
     (string-append (date->string now #t) "Z"))
   (header-username parent-header)
   (if msg-type msg-type (reply-type (header-msg-type parent-header)))))

(define (reply-type parent-type)
  (case parent-type
    [(kernel_info_request) 'kernel_info_reply]
    [(comm_info_request) 'comm_info_reply]
    [(execute_request) 'execute_reply]
    [(complete_request) 'complete_reply]
    [(is_complete_request) 'is_complete_reply]
    [(object_info_request) 'object_info_reply]
    [(shutdown_request) 'shutdown_reply]
    [(history_request) 'history_reply]
    [else (error 'reply-type "no reply for message type: ~e" parent-type)]))

;; Message types
(define shell-in-message-type/c
  (or/c 'kernel_info_request
        'comm_info_request
        'execute_request
        'complete_request
        'is_complete_request
        'object_info_request
        'shutdown_request
        'history_request))

(define shell-out-message-type/c
  (or/c 'kernel_info_reply
        'comm_info_reply
        'execute_reply
        'complete_reply
        'is_complete_reply
        'object_info_reply
        'shutdown_reply
        'history_reply
        'execute_result))

(define iopub-message-type/c
  (or/c 'display_data
        'execute_input
        'execute_result
        'status
        'clear_output
        'stream))

(define comm-message-type/c
  (or/c 'comm_open
        'comm_msg
        'comm_close))

(define stdin-message-type/c
  (or/c 'input_reply
        'input_request))

(define message-type/c
  (or/c shell-in-message-type/c
        shell-out-message-type/c
        iopub-message-type/c
        comm-message-type/c
        stdin-message-type/c))

;; Mime-types that might show up in output to Jupyter notebooks.
(define mime-type/c
  (or/c 'application/json
        'application/pdf
        'application/xml
        'image/gif
        'image/jpeg
        'image/png
        'image/bmp
        'image/svg+xml
        'image/tiff
        'text/csv
        'text/html
        'text/markdown
        'text/plain
        'text/rtf
        'text/xml
        'video/avi
        'video/mpeg
        'video/mp4))

;; type Header
(define-struct/contract header
  ([identifiers (listof bytes?)]
   [parent-header any/c] ;; (recursive-contract (or/c false/c header?))]
   [metadata jsexpr?] ;; TODO hash table?
   [message-id string?] ;; uuid-string?
   [session-id string?] ;; uuid-string?
   [date (or/c string? #f)] ;; ISO 8601 date string
   [username string?]
   [msg-type message-type/c])
  #:transparent)

;; type Message
(define-struct/contract message
  ([header header?]
   [content jsexpr?])
  #:transparent)

;; message-ref : Message Symbol [Any] -> Any
(define (message-ref msg key [default (mk-message-ref-error msg key)])
  (hash-ref (message-content msg) key default))
(define ((mk-message-ref-error msg key))
  (error 'message-ref "key not found\n  key: ~e\n  message: ~e" key msg))

;; ============================================================
;; Communication

(provide (struct-out config)
         (contract-out
          [read-config
           (-> config?)]))

;; Jupyter's ZeroMQ binding configuration.
(define-struct/contract config
  ([control-port exact-nonnegative-integer?]
   [shell-port exact-nonnegative-integer?]
   [transport string?]
   [signature-scheme (or/c 'hmac-sha256)]
   [stdin-port exact-nonnegative-integer?]
   [hb-port exact-nonnegative-integer?]
   [ip string?]
   [iopub-port exact-nonnegative-integer?]
   [key bytes?])
  #:transparent)

;; read-config : -> Config
;; Parses a Jupyter configuration from (current-input-port).
(define (read-config)
  (define config-json (read-json))
  (make-config
   (hash-ref config-json 'control_port)
   (hash-ref config-json 'shell_port)
   (hash-ref config-json 'transport)
   (string->symbol (hash-ref config-json 'signature_scheme))
   (hash-ref config-json 'stdin_port)
   (hash-ref config-json 'hb_port)
   (hash-ref config-json 'ip)
   (hash-ref config-json 'iopub_port)
   (string->bytes/utf-8 (hash-ref config-json 'key))))

;; connection-key : (Parameterof (U #f Bytes))
(define connection-key (make-parameter #f))

;; Delimeter between Jupyter ZMQ message identifiers and message body.
(define message-delimiter #"<IDS|MSG>")

;; message->frames : Message -> (Listof Bytes)
(define (message->frames msg)
  (define header (message-header msg))
  (define idents (header-identifiers header))
  (define header-bytes (jsexpr->bytes (header->jsexpr header)))
  (define parent-bytes (jsexpr->bytes (header->jsexpr (header-parent-header header))))
  (define metadata (jsexpr->bytes (header-metadata header)))
  (define content (jsexpr->bytes (message-content msg)))
  (define key (connection-key))
  (define sig (if key (hash-message key header-bytes parent-bytes metadata content) #""))
  (append idents (list message-delimiter sig header-bytes parent-bytes metadata content)))

;; frames->message : (Listof Bytes) -> Message
(define (frames->message frames)
  (define-values (idents frames2)
    (let loop ([acc null] [frames frames])
      (cond [(equal? (car frames) message-delimiter)
             (values acc (cdr frames))]
            [else (loop (cons (car frames) acc) (cdr frames))])))
  (match-define (list sig header-data parent-header metadata content) frames2)
  (parse-message sig idents header-data parent-header metadata content))

(define (parse-message sig idents header-bytes parent-header metadata content)
  (define key (connection-key))
  (define verif-sig (hash-message key header-bytes parent-header metadata content))
  (unless (or (not key) (equal? sig verif-sig))
    (error "Message from unauthenticated user."))
  (make-message
   (parse-header idents (bytes->jsexpr header-bytes) (bytes->jsexpr parent-header) metadata)
   (bytes->jsexpr content)))

;; helpers for parsing/unparsing messages
(define (parse-header idents header parent-header metadata)
  (define parent-result
    (cond [(hash-empty? parent-header) #f]
          [else (parse-header idents parent-header (hasheq) metadata)]))
  (make-header
   idents
   parent-result
   (bytes->jsexpr metadata)
   (hash-ref header 'msg_id)
   (hash-ref header 'session)
   (hash-ref header 'date #f)
   (hash-ref header 'username)
   (string->symbol (hash-ref header 'msg_type))))

(define (hash-message key header-data parent-header metadata content)
  (define data (bytes-append header-data parent-header metadata content))
  (string->bytes/utf-8 (bytes->hex-string (hmac-sha256 key data))))

(define (header->jsexpr hd)
  (define js
    (cond [hd (hasheq
               'msg_id (header-message-id hd)
               'username (header-username hd)
               'session (header-session-id hd)
               'date (header-date hd)
               'msg_type (symbol->string (header-msg-type hd))
               'version "5.0")]
          [else (hasheq)]))
  (if (hash-ref js 'date) js (hash-remove js 'date)))

;; ----

;; receive-message! : ZMQ-Socket -> Message
;; Receives an Jupyter message on the given socket.
(define (receive-message! socket)
  (frames->message (zmq-recv* socket)))

;; send-message! : ZMQ-Socket Message -> Void
;; Sends the given Jupyter message on the given socket.
(define (send-message! socket msg)
  (zmq-send* socket (message->frames msg)))


;; ============================================================
;; Kernel

(provide (contract-out
          [run-kernel
           (-> config? (-> services? handler-table/c) any)]))

;; A running kernel consists of multiple "service" threads plus a main worker
;; thread. The service threads communicate with the Jupyter front end via ZMQ
;; sockets. When a service thread receives a message that requires the kernel to
;; do work, it forwards the request message to the worker thread, waits for the
;; response, and then forwards the response message back to the Jupyter front
;; end.
;;
;; Communication between service threads and the worker thread is done using
;; async thread mailboxes (thread-send, thread-receive). The service thread
;; sends the worker thread a pair containing the request message and the thread
;; to which the response should be sent (typically the service thread itself).
;;
;;   Service -> Worker         : (cons RequestMessage RespondToThread)
;;   Worker -> RespondToThread : ResponseMessage
;;
;; The worker thread handles requests according to a table of handlers keyed by
;; message type. Unhandled messages are ignored.

(define (run-kernel cfg make-handlers)
  (parameterize ([connection-key (config-key cfg)])
    (call-with-services cfg
      (λ (services)
        (run-worker services (add-default-handlers cfg (make-handlers services)))))))

;; ------------------------------------------------------------
;; Services

(provide (contract-out
          [make-stdin-port
           (->* [services? message?] [any/c] input-port?)]
          [make-stream-port
           (-> services? (or/c 'stdout 'stderr) message? output-port?)]
          [send-exec-result
           (-> message? services? exact-integer? jsexpr? void?)]))

(define-struct/contract services
  ([heartbeat thread?]
   [shell thread?]
   [control thread?]
   [stdin thread?]
   [iopub thread?])
  #:transparent)

;; call-with-services : Config (Services -> X) -> X ???
;; Setup services, call action, and close services before returning.
(define (call-with-services cfg action)
  (define worker (current-thread))
  (define cust (make-custodian))
  (define (serve socket-type port action)
    (define endpoint (format "~a://~a:~a" (config-transport cfg) (config-ip cfg) port))
    (define socket (zmq-socket socket-type #:bind endpoint))
    (thread (lambda () (action socket worker))))
  (begin0 (parameterize ((current-custodian cust))
            (define services
              (make-services
               (serve 'rep    (config-hb-port cfg)      heartbeat)
               (serve 'router (config-shell-port cfg)   shell)
               (serve 'router (config-control-port cfg) control)
               (serve 'router (config-stdin-port cfg)   stdin)
               (serve 'pub    (config-iopub-port cfg)   iopub)))
            (action services))
    (custodian-shutdown-all cust)))

;; ----

;; send-status : Services Header (U 'idle 'busy) -> Void
(define (send-status services parent-header status)
  (define header (make-response-header parent-header #:msg-type 'status))
  (define msg (make-message header (hasheq 'execution_state (symbol->string status))))
  (thread-send (services-iopub services) msg))

;; send-exec-result : Message Services Nat JSExpr -> Void
(define (send-exec-result msg services execution-count data)
  (thread-send (services-iopub services)
               (make-response msg (hasheq 'execution_count execution-count
                                          'data data
                                          'metadata (hasheq))
                              #:msg-type 'execute_result)))

;; make-stdin-port : Services Message [Any] -> InputPort
;; custom input-port which sends & receives stdin messages
(define (make-stdin-port services msg [name 'stdin])
  (make-fetch-input-port name (lambda () (request-stdin-from-frontend msg services))))

;; request-stdin-from-frontend : Message Services -> String
(define (request-stdin-from-frontend msg services)
  (thread-send (services-stdin services)
               (cons (make-response msg (hasheq 'prompt ""
                                                'password #f
                                                'metadata (hasheq))
                                    #:msg-type 'input_request)
                     (current-thread)))
  (define msg-reply (thread-receive))
  (define response (message-ref msg-reply 'value))
  (string-append response "\n"))

;; make-fetch-input-port : Any (-> String) -> InputPort
(define (make-fetch-input-port name fetch)
  (define-values (pipe-in pipe-out) (make-pipe))
  (define (read-in buf)
    (if (sync/timeout 0 pipe-in)
        pipe-in
        (let ([more (fetch)])
          (write-string more pipe-out)
          (read-in buf))))
  (define (close)
    (close-input-port pipe-in)
    (close-output-port pipe-out))
  (make-input-port/read-to-peek name read-in #f close))

;; make-stream-port : Services (U 'stdout 'stderr) Message -> OutputPort
(define (make-stream-port services name orig-msg)
  (define iopub (services-iopub services))
  (define-values (port-name stream-name)
    (case name
      [(stdout) (values "pyout" "stdout")]
      [(stderr) (values "pyerr" "stderr")]))
  (define (send-stream str)
    (thread-send iopub
                 (make-response orig-msg (hasheq 'name stream-name 'text str)
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
(define ((shell-like who) socket worker)
  (let loop ()
    (define msg (receive-message! socket))
    (thread-send worker (cons msg (current-thread)))
    (define response (thread-receive))
    (when response (send-message! socket response))
    (loop)))

;; implements stdin protocol
(define ((stdin-like who) socket worker)
  (let loop ()
    (match-define (cons msg executor-thread) (thread-receive))
    (send-message! socket msg)
    (define response (receive-message! socket))
    (when response (thread-send executor-thread response))
    (loop)))

(define shell (shell-like 'shell))
(define stdin (stdin-like 'stdin))
(define control (shell-like 'control))

(define (iopub socket worker)
  (let loop ()
    (define msg (thread-receive))
    (when msg (send-message! socket msg))
    (loop)))

;; ----------------------------------------
;; Connect (deprecated since Jupyter 5.1)

(define (connect cfg)
  (hasheq
   'shell_port (config-shell-port cfg)
   'iopub_port (config-iopub-port cfg)
   'stdin_port (config-stdin-port cfg)
   'hb_port (config-hb-port cfg)))

;; ------------------------------------------------------------
;; Worker Handlers

(define handler/c (-> message? jsexpr?))
(define handler-table/c (hash/c symbol? handler/c))

;; run-worker : Services HandlerTable -> Void
(define (run-worker services the-handlers)
  (let loop ()
    (match-define (cons msg respond-to) (thread-receive))
    (send-status services (message-header msg) 'busy)
    (define-values (response shutdown?) (handle the-handlers msg))
    (thread-send respond-to response)
    (send-status services (message-header msg) 'idle)
    (unless shutdown? (loop))))

(define (handle handlers msg)
  (define msg-type (header-msg-type (message-header msg)))
  (define handler (hash-ref handlers msg-type #f))
  (when #f (unless handler (eprintf "handle: unknown message type: ~e\n" msg-type)))
  (values (and handler (make-response msg (handler msg)))
          (eq? 'shutdown_request msg-type)))

(define (make-default-handlers cfg)
  (hasheq 'connect_request (lambda (msg) (connect cfg))
          'shutdown_request (lambda (msg) (hasheq 'restart #f))
          'comm_info_request (lambda (msg) (hasheq))
          'is_complete_request (lambda (msg) (hasheq 'status "unknown"))))

(define (add-default-handlers cfg h)
  (for/fold ([h h]) ([(k v) (in-hash (make-default-handlers cfg))])
    (if (hash-has-key? h k) h (hash-set h k v))))
