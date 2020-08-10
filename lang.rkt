#lang racket/base
(require (for-syntax racket/base)
         racket/match racket/port)
(provide iracket-module-begin)

;; Like racket/load ("The top level is hopeless!"), but
;; - allows control over the initial language
;; - allows control over the reader
;; - read is delayed until run-time and incremental, so evaluation of
;;   one term can affect reading of next term
;; - interpreted specially by IRacket kernel: set sandbox namespace and reader

;; Usage:
;;
;;   #lang iracket/lang <langopt> ...
;;   where langopt =
;;   | #:language <modulepath>
;;   | #:reader <readerspec>
;;
;; Example:
;;   #lang iracket/lang #:language racket
;;   #lang iracket/lang #:reader ??? #:language scribble/doc

(module reader racket/base
  (require racket/port)
  (provide (rename-out [-read read] [-read-syntax read-syntax]))
  (define (-read in)
    (syntax->datum (-read-syntax #f in)))
  (define (-read-syntax src in)
    (define-values (line col pos) (port-next-location in))
    (define content (port->bytes in))
    (define loc (list src line col pos #f))
    (define mod-decl
      `(module anonymous-module iracket/lang
         (iracket-module-begin ,(datum->syntax #f content loc))))
    (datum->syntax #f mod-decl)))

(module config racket/base
  (require racket/match racket/port)
  (provide (all-defined-out))

  (define (read-lang-config in)
    (define (bad fmt . args) (apply error 'iracket/lang fmt args))
    (define line0-in (open-input-bytes (read-bytes-line in 'any)))
    (define args (port->list read line0-in))
    (let loop ([args args] [config (hasheq)])
      (match args
        [(list* '#:require language rest)
         (when (hash-has-key? config 'language)
           (bad "duplicate #:language argument\n  got: ~.s" language))
         (unless (module-path? language)
           (bad "expected module path for language\n  got: ~.s" language))
         (loop rest (hash-set config 'language language))]
        [(list* '#:reader reader rest)
         (when (hash-has-key? config 'reader)
           (bad "duplicate #:reader argument\n  got: ~.s" reader))
         (unless (module-path? reader)
           (bad "expected module path for reader\n  got: ~.s" reader))
         (loop rest (hash-set config 'reader reader))]
        ['() config])))

  (define (config:get-language config)
    (hash-ref config 'language 'racket))

  (define (config:get-read-syntax config ns)
    (parameterize ((current-namespace ns))
      (wrap-reader
       (cond [(hash-ref config 'reader #f)
              => (lambda (reader) (dynamic-require reader 'read-syntax))]
             [else read-syntax]))))

  (define (wrap-reader read-stx)
    (lambda (src in) (read-stx src in))))

(require 'config)

;; ----------------------------------------

(define-syntax (iracket-module-begin stx)
  (syntax-case stx ()
    [(_ cell ...)
     (with-syntax ([the-namespace (datum->syntax stx 'the-namespace)])
       #'(#%plain-module-begin
          (provide the-namespace)
          (define the-namespace
            (iracket-run (#%variable-reference) (quote-syntax cell) ...))))]))

(define (iracket-run varref stx . more-stxs)
  (define src (syntax-source stx))
  (define in (open-syntax-bytes-port stx))
  (define config (read-lang-config in))
  (define ns (variable-reference->empty-namespace varref))
  (parameterize ((current-namespace ns))
    (namespace-require (config:get-language config))
    (define read-stx (config:get-read-syntax config ns))
    (let loop ()
      (define next (read-stx src in))
      (unless (eof-object? next)
        (call-with-values
         (lambda () (eval-top next))
         (lambda vs (for ([v (in-list vs)] #:unless (void? v)) (println v))))
        (loop)))
    ns))

(define (open-syntax-bytes-port stx)
  (define in (open-input-bytes (syntax-e stx)))
  (port-count-lines! in)
  (let ([l (syntax-line stx)] [c (syntax-column stx)] [p (syntax-position stx)])
    (set-port-next-location! in l c p))
  in)

(define (eval-top e)
  (call-with-continuation-prompt
   (lambda () (eval e))))
