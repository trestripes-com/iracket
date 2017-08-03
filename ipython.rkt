#lang racket/base

(require racket/string
         racket/contract
         racket/match
         json
         sha
         zeromq
         "ipython-message.rkt")

(provide (struct-out config)
         read-config
         connection-key
         receive-message!
         send-message!)

;; IPython's ZeroMQ binding configuration.
(define-struct/contract config
  ([control-port exact-nonnegative-integer?]
   [shell-port exact-nonnegative-integer?]
   [transport string?]
   [signature-scheme (symbols 'hmac-sha256)]
   [stdin-port exact-nonnegative-integer?]
   [hb-port exact-nonnegative-integer?]
   [ip string?]
   [iopub-port exact-nonnegative-integer?]
   [key bytes?])
  #:transparent)

;; Parses an IPython configuration from (current-input-port).
(define/contract (read-config)
  (-> config?)
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


;; Key for the connection to IPython. (or/c bytes? false/c).
(define connection-key (make-parameter #f))

;; Delimeter between IPython ZMQ message identifiers and message body.
(define message-delimiter #"<IDS|MSG>")

;; Receives an IPython message on the given socket.
(define/contract (receive-message! socket)
  (zmq-socket? . -> . message?)
  (define frames (zmq-recv* socket))
  (define-values (idents frames2)
    (let loop ([acc null] [frames frames])
      (cond [(equal? (car frames) message-delimiter)
             (values acc (cdr frames))]
            [else (loop (cons (car frames) acc) (cdr frames))])))
  (match-define (list sig header-data parent-header metadata content) frames2)
  (parse-message sig idents header-data parent-header
                 metadata content))

;; Sends the given IPython message on the given socket.
(define/contract (send-message! socket msg)
  (zmq-socket? message? . -> . void?)
  (define header (message-header msg))
  (define idents (header-identifiers header))
  (define header-bytes (jsexpr->bytes (header->jsexpr header)))
  (define parent-bytes (jsexpr->bytes (header->jsexpr (header-parent-header header))))
  (define metadata (jsexpr->bytes (header-metadata header)))
  (define content (jsexpr->bytes (message-content msg)))
  (define key (connection-key))
  (define sig
    (cond [key (hash-message (connection-key)
                            header-bytes parent-bytes metadata content)]
          [else (string->bytes/utf-8 "")]))
  (zmq-send* socket
             (append idents
                     (list message-delimiter
                           sig
                           header-bytes
                           parent-bytes
                           metadata
                           content))))

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
   (hash-ref header 'username)
   (string->symbol (hash-ref header 'msg_type))))

(define (parse-message sig idents header-bytes parent-header metadata content)
  (define key (connection-key))
  (define verif-sig (hash-message key header-bytes parent-header metadata content))
  (unless (or (not key) (equal? sig verif-sig))
    (error "Message from unauthenticated user."))
  (make-message
   (parse-header idents (bytes->jsexpr header-bytes) (bytes->jsexpr parent-header) metadata)
   (bytes->jsexpr content)))

(define (hash-message key header-data parent-header metadata content)
  (define data (bytes-append header-data parent-header metadata content))
  (string->bytes/utf-8 (bytes->hex-string (hmac-sha256 key data))))

(define (header->jsexpr hd)
  (cond [hd (hasheq
             'msg_id (header-message-id hd)
             'username (header-username hd)
             'session (header-session-id hd)
             'msg_type (symbol->string (header-msg-type hd))
             'version "5.0")]
        [else (hasheq)]))
;; end helpers for parsing messages
