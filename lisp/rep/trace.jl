;;  M. Maruska


;; fixme: broken!
(define-structure rep.trace
    ;; sawfish.user.
    (export
      print-debug
      DB
					;message-format
      )
    (open
     rep
     rep.system
     rep.io.files                       ;stderr-file
     ;;rep.io.file-handlers

     )

  (define debug #t)

  (define (print-debug fmt #!rest arguments)
					;(message (apply format #f fmt arguments))
    (apply format (stderr-file) fmt arguments))

  (defmacro DB (fmt #!rest arguments)
    ;; fixme:
					; `(if ,debug (print-debug ,fmt ,@arguments))
    `(if debug
	 (print-debug ,fmt ,@arguments)
       ))

  )
