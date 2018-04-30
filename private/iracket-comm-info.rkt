#lang racket/base

(provide comm-info)

;; comm_info_request
;; replies with an empty dictionary for the comm_info_request
(define comm-info
  (hasheq
   'comms ""))
