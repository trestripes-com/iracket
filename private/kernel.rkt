#lang racket/base
(require (for-syntax racket/base)
         racket/string
         racket/match
         racket/contract
         racket/sandbox
         racket/pretty
         racket/port
         file/convertible
         xml
         json
         net/base64
         "jupyter.rkt")

;; ============================================================
;; Info

(provide kernel-info comm-info)

(define kernel-info
  (hasheq
   'language_info (hasheq
                   'mimetype "text/x-racket"
                   'name "Racket"
                   'version (version)
                   'file_extension ".rkt"
                   'pygments_lexer "racket"
                   'codemirror_mode "scheme")

   'implementation "iracket"
   'implementation_version "1.0"
   'protocol_version "5.0"
   'language "Racket"
   'banner "IRacket 1.0"
   'help_links (list (hasheq
                      'text "Racket docs"
                      'url "http://docs.racket-lang.org"))))

(define comm-info
  (hasheq))

;; ============================================================
;; Completion

(provide (contract-out
          [complete
           (-> any/c message? jsexpr?)]
          [is-complete-request
           (-> any/c message? jsexpr?)]))

;; complete : Evaluator Message -> JSExpr
(define (complete e msg)
  (define code (hash-ref (message-content msg) 'code))
  (define cursor-pos (hash-ref (message-content msg) 'cursor_pos))
  (define prefix (car (regexp-match #px"[^\\s,)(]*$" code 0 cursor-pos)))
  (define suffix (car (regexp-match #px"^[^\\s,)(]*" code (sub1 cursor-pos))))
  (define words (call-in-sandbox-context e namespace-mapped-symbols))
  (define matches
    (sort (filter (λ (w) (string-prefix? prefix w))
                  (map symbol->string words))
          string<=?))
  (hasheq
   'matches matches
   'cursor_start (- cursor-pos (string-length prefix))
   'cursor_end (+ cursor-pos (string-length suffix) -1)
   'status "ok"))

;; is-complete-request : Evaluator Message -> JSExpr
(define (is-complete-request e msg)
  (define code (hash-ref (message-content msg) 'code))
  (hasheq 'status "unknown"))

(define (string-prefix? prefix word)
  (and (<= (string-length prefix) (string-length word))
       (for/and ([c1 (in-string prefix)] [c2 (in-string word)]) (eqv? c1 c2))))

;; ============================================================
;; Execute

(provide make-execute)

;; make-execute : Evaluator -> Services -> Message -> JSExpr
(define ((make-execute e) services)
  (define execution-count 0)
  (define user-cust (get-user-custodian e))
  ;; let other cells' threads be killed
  (call-in-sandbox-context e
   (λ ()
     (eval
      `(define notebook-kill-thread
         ,(make-kill-thread/custodian user-cust)))))
  (λ (msg)
    (set! execution-count (add1 execution-count))
    (define code (hash-ref (message-content msg) 'code))
    (define allow-stdin (hash-ref (message-content msg) 'allow_stdin))
    (call-in-sandbox-context e
     (λ ()
       (when allow-stdin (current-input-port (make-stdin-port services msg)))
       (current-output-port (make-stream-port services 'stdout msg))
       (current-error-port (make-stream-port services 'stderr msg))))
    (call-with-values
     (λ () (e code))
     (λ vs
       (match vs
         [(list (? void?)) void]
         [else (for ([v (in-list vs)])
                 (define results (make-display-results v))
                 (send-exec-result msg services execution-count
                                   (make-hasheq results)))])))
    (hasheq
     'status "ok"
     'execution_count execution-count
     'user_expressions (hasheq))))

(define (make-display-text v)
  (parameterize ((print-graph #t))
    (cons 'text/plain (format "~v" v))))

(define (make-display-html v)
  (define-values (pin pout) (make-pipe-with-specials))
  (define (size-hook v d? out)
    (cond [(and (convertible? v) (convert v 'png-bytes)) 1]
          [else #f]))
  (define (print-hook v d? out)
    (define png-data (convert v 'png-bytes))
    (define img-src (format "data:image/png;base64,~a" (base64-encode png-data)))
    (define img-style
      "display: inline; vertical-align: baseline; padding: 0pt; margin: 0pt; border: 0pt")
    (write-special `(img ((style ,img-style) (src ,img-src))) out))
  (parameterize ((pretty-print-columns 'infinity)
                 (pretty-print-size-hook size-hook)
                 (pretty-print-print-hook print-hook))
    (pretty-print v pout))
  (close-output-port pout)
  ;; read-contents : InputPort[Special] -> (Listof (U String Special))
  (define (read-contents in)
    (define buf (make-bytes 1000))
    (define (bs->string bs)
      (bytes->string/utf-8 (apply bytes-append (reverse bs))))
    (define (loop acc bs)
      (define next (read-bytes-avail! buf in))
      (cond [(eof-object? next)
             (let ([acc (cons (bs->string bs) acc)])
               (reverse acc))]
            [(exact-positive-integer? next)
             (loop acc (cons (subbytes buf 0 next) bs))]
            [(procedure? next)
             (let ([acc (cons (bs->string bs) acc)])
               (loop (cons (next #f #f #f #f) acc) null))]))
    (loop null null))
  (cons 'text/html (xexpr->string `(code ,@(read-contents pin)))))

(define (make-display-convertible conversion-type mime-type v
                                  #:encode [encode values])
  (define result (and (convertible? v) (convert v conversion-type)))
  (if result
      (cons mime-type (bytes->string/latin-1 (encode result)))
      #f))

(define (make-display-c3 v)
  (match v
    [`(c3-data . ,d) (cons 'application/x-c3-data (jsexpr->string d))]
    [else #f]))

(define (make-display-results v)
  (filter values
          (list (make-display-c3 v)
                (make-display-html v)
                (make-display-text v))))

(define (make-kill-thread/custodian cust)
  (λ (t)
    (parameterize ([current-custodian cust])
      (kill-thread t))))
