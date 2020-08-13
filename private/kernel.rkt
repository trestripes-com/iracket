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

(provide kernel-info)

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

;; ============================================================
;; Completion

(provide (contract-out
          [complete
           (-> any/c message? jsexpr?)]))

;; complete : Evaluator Message -> JSExpr
(define (complete e msg)
  (define code (message-ref msg 'code))
  (define cursor-pos (message-ref msg 'cursor_pos))
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

(define (string-prefix? prefix word)
  (and (<= (string-length prefix) (string-length word))
       (for/and ([c1 (in-string prefix)] [c2 (in-string word)]) (eqv? c1 c2))))

;; ============================================================
;; Execute

(require (submod "../lang.rkt" config))

(provide make-execute
         current-kernel-make-namespace)

;; current-kernel-make-namespace : parameter of (module-path? -> namespace?)
;; Used by execute & "#lang iracket/lang" to make a new namespace that shares
;; the original's root namespace (and thus module instances) and attached
;; modules. PRE: run in sandbox context
(define current-kernel-make-namespace (make-parameter #f))

;; make-execute : Evaluator -> Services -> Message -> JSExpr
(define ((make-execute e) services)
  (define execution-count 0)
  (λ (msg)
    (set! execution-count (add1 execution-count))
    (define code (message-ref msg 'code))
    (define allow-stdin (message-ref msg 'allow_stdin))
    (match (let ([in (if allow-stdin (make-stdin-port services msg) (null-input-port))]
                 [out (make-stream-port services 'stdout msg)]
                 [err (make-stream-port services 'stderr msg)])
             (do-execute e code in out err))
      ;; Note: if the evaluator is created with (sandbox-propagate-exceptions #f), then
      ;; the raise path should never be used; the exception is printed to stderr instead.
      [(list 'raise e)
       ;; Jupyter notebook (v5.2.2) seems to only display traceback; it ignores
       ;; ename and evalue, so give them dummy "error" values.
       ;; See also https://github.com/jupyter/jupyter_client/issues/363
       (define traceback (raised->traceback e))
       (unless (message-ref msg 'silent #f)
         (send-exec-error msg services execution-count "error" "error" traceback))
       (hasheq 'status "error"
               'execution_count execution-count
               'ename "error"
               'evalue "error"
               'traceback traceback)]
      [(cons 'values vs)
       (unless (message-ref msg 'silent #f)
         (for ([v (in-list vs)] #:when (not (void? v)))
           (define results (make-display-results v))
           (send-exec-result msg services execution-count (make-hasheq results))))
       (hasheq 'status "ok"
               'execution_count execution-count
               'user_expressions (hasheq))])))

(define (do-execute e code0 in out err)
  (define lang-rx #px"^#lang iracket/lang(?:[\\s]|$)")
  (define code
    (cond [(regexp-match? lang-rx code0)
           (define code-in (open-input-string code0))
           (port-count-lines! code-in)
           (void (regexp-match lang-rx code-in)) ;; consume from input
           (define config (read-lang-config code-in))
           (call-in-sandbox-context e
             (lambda ()
               (define ns ((current-kernel-make-namespace) (config:get-language config)))
               (current-namespace ns)
               (define read-stx (config:get-read-syntax config ns))
               (sandbox-reader (lambda (src)
                                 (port->list (lambda (in) (read-stx src in))
                                             (current-input-port))))))
           code-in]
          [else code0]))
  (with-handlers ([(lambda (e) #t) (lambda (e) (list 'raise e))])
    (call-in-sandbox-context e
      (λ ()
        (current-input-port in)
        (current-output-port out)
        (current-error-port err)))
    (call-with-values (lambda () (e code))
                      (lambda vs (cons 'values vs)))))

(define (raised->traceback e)
  (cond [(exn? e)
         (let ([out (open-output-string)])
           (parameterize ((current-error-port out))
             ((error-display-handler) (exn-message e) e))
           (string-split (get-output-string out) "\n"))]
        [else (list (format "not an exception: ~e" e))]))

(define (null-input-port) (open-input-bytes #""))

(define (make-display-results v)
  (filter values
          (list (make-display-html v)
                (make-display-text v))))

;; ----

(define (make-display-text v)
  (parameterize ((print-graph #t))
    (cons 'text/plain (format "~v" v))))

(define (make-display-html v)
  (define pin (value->special-html-input-port v))
  (cons 'text/html (xexpr->string `(code ,@(port->contents pin)))))

;; port->contents : InputPort[special=X] -> (Listof (U String X))
(define (port->contents in)
  (define buf (make-bytes 1000))
  (define out (open-output-bytes))
  (let loop ([acc null])
    (define next (read-bytes-avail! buf in))
    (cond [(exact-positive-integer? next)
           (write-bytes buf out 0 next)
           (loop acc)]
          [else
           (let* ([str (bytes->string/utf-8 (get-output-bytes out #t))]
                  [acc (if (zero? (string-length str)) acc (cons str acc))])
             (cond [(eof-object? next)
                    (reverse acc)]
                   [(procedure? next)
                    (loop (cons (next #f #f #f #f) acc))]))])))

;; value->special-html-input-port : Any -> InputPort
;; Pretty-prints the value to a pipe and returns the input end. Pretty-printing
;; produces mostly text, but images (and other convertible things) are embedded
;; as xexpr specials in the port.
(define (value->special-html-input-port v)
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
  pin)
