#lang racket/base

;; Jupyter kernel for Racket
;; You probably don't want to run this directly.

(require racket/list
         racket/contract
         racket/sandbox
         "private/kernel.rkt"
         "private/jupyter.rkt")

(provide main)

(define (main config-file-path)
  ;; Jupyter hides stdout, but prints stderr, so use eprintf for debugging.
  (eprintf "Kernel starting.\n")
  (define cfg (with-input-from-file config-file-path read-config))
  (define evaluator (create-evaluator cfg))
  (run-kernel cfg
              (lambda (services)
                (hasheq 'kernel_info_request (lambda (msg) kernel-info)
                        'execute_request ((make-execute evaluator) services)
                        'complete_request (lambda (msg) (complete evaluator msg)))))
  (eprintf "Kernel terminating.\n"))

(define (create-evaluator cfg)
  (parameterize ([sandbox-eval-limits (list #f #f)]
                 [sandbox-memory-limit #f]
                 [sandbox-propagate-exceptions #f]
                 [sandbox-namespace-specs (list sandbox-make-namespace 'file/convertible)]
                 [sandbox-path-permissions (list (list 'read "/"))])
    (make-evaluator '(begin))))
