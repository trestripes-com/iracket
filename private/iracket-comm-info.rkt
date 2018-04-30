#lang racket/base

(provide comm-info)

;; comm_info_request / comm_info_reply
;; A complete request / replyi template looks like:
;; comm_info_request
;; content = {
;;     # Optional, the target name
;;     'target_name': str,
;; }
;; comm_info_reply
;; content = {
;;     # A dictionary of the comms, indexed by uuids.
;;     'comms': {
;;         comm_id: {
;;             'target_name': str,
;;         },
;;     },
;; }
;;
;; In this case, simply reply with an empty dictionary 
(define comm-info
  (hasheq))
