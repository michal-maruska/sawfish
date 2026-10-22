;; (C) 2002,2003,2004  Michal Maruska mmc@maruska.dyndns.org

;; this module implements a system for symbolic hooks:
;; functions in a hook can by associated with a symbol, and you can access/modify the function
;; associated with the symbol.

;; [02 nov 04]   hook should be an object. But unfortunately invoking hooks is in C, so
;; we bow.
;; removing is hacky then: we have to pass the symbol, not the value!!!



;; scheme doesn't have the same relation (as common lisp) between symbols
;; and functions, so functions are _not_ accessed via another redirection.  Also,
;; librep lacks any tool to redefine modules.

;; the mapping presented here (symbol -> function) is stored in the property list of the
;; symbol, the hook variable.  Scheme doesn't have such thing (variable is a
;; symbol with property list), but this is `librep', a mix of scheme and elisp.

;;  add-window-hook  ->   function1  function2 function3 .....
;;    |
;;    | property list
;;    v
;;  'keymap-add-window -> function3
;;  'reframe-window    -> function2
;;

;; So, to identify the functions in the hook, i associate a symbol with
;; them. This symbol in most cases can be the function name.  so, instead of
;; calling (add-hook 'enter-workspace-hook update-edges) you can simply call a
;; `MACRO' (add-hook-s 'enter-workspace-hook update-edges) which will
;; symbolically tag the function inside the hook as 'update-edges.  Note, that it
;; will replace the previous (if any) function under that tag.

;; what other operations are possible:

;; [02 nov 04]  `NEW' names:

;; add-hook-under-symbol
;; remove-hook-under-symbol
;; list-hook-symbols
;; should i give a name to those symbols?  like  `features'?




;; `add-hook-s' is a macro (obviously), which calls `mm-add-hook'.  You need to
;; call it directly, if you have a simple anonymous lambda.

;; the interesting calls:
;; `mm-remove-hook'     .... remove the function given by its tag inside the hook
;; `mm-hook-under-symbol' ...

;; `add-to-list-under-symbol' is exported b/c it is used by
;; `mm-call-after-property-changed' and `mm-call-after-state-changed' in sawfish.wm.windows

(define-structure rep.mmsystem
    (export

      ;; new names:
      add-hook-under-symbol
      remove-hook-under-symbol
      list-hook-symbols


      ;; old, obsolete

      mm-add-hook                       ;  setter
      mm-remove-hook                    ; substitute for `remove-hook'
      mm-hook-under-symbol
      mm-remove-hook-symbol

      ;; for compatibility: and convenience!
      add-hook-s                        ; substitute for `add-hook'
     add-to-list-under-symbol
      )
    (open
     rep
     rep.system                         ;message
     rep.io.files
     rep.data
     rep.system                         ; here original ...
     rep.data.symbol-table
     rep.data.tables)

  (eval-when-compile
    (define debug #f))
  (define debug #f)

  (define (print-debug fmt #!rest arguments)
					;(message (apply format #f fmt arguments))
    (apply format (stderr-file) fmt arguments))

  (defmacro DB (fmt #!rest arguments)
    ;; fixme:
					; `(if ,debug (print-debug ,fmt ,@arguments))
    (if (and (boundp 'debug) debug)
	`(print-debug ,fmt ,@arguments)
      '()))

;; i use these:
  (define (mm-make-table)
    ;; eq-hash eq make-symbol-table
    (make-weak-table eq-hash eq))


  ;;; given a variable/symbol  `list-symbol', add  `item' to the value of the symbol/variable (which should be a list), and to a hash table
  ;;; associated w/ the variable/symbol as a symbol property under `under-symbol'

  ;; example:  (add-to-list-under-symbol hook-symbol new-func at-end under-symbol 'hook-mapping remove-hook)
  (define (add-to-list-under-symbol list-symbol item #!optional at-end under-symbol
				    (under-property 'hook-mapping)
				    (remove-function remove-hook)
				    #!key (table #f))
    "Arrange it so that FUNCTION-NAME is added to the hook-list stored in
symbol, LIST-SYMBOL. It will added at the head of the list unless AT-END
is true in which case it is added at the end."

    (DB "add-to-list-under-symbol: to: %s under: %s  property: %s \n" list-symbol under-symbol under-property)

    (unless table
      ;; i need it a symbol. With a proplist
      (unless (boundp list-symbol)
	(DB "add-to-list-under-symbol: new symbol! %s\n" list-symbol)
	(make-variable-special list-symbol) ; it already is normally
	(set list-symbol nil))

      ;; why ??
      ;; (if under-symbol

      ;; make a prop-list
      (setq table
	    (let ((hash (get list-symbol under-property))) ; proplist
	      (unless hash
		(DB "add-to-list-under-symbol: we must create the table for %s\n" list-symbol)
		(setq hash  (mm-make-table))
		(put list-symbol under-property hash))
	      hash)))

    (DB "add-to-list-under-symbol: table ready\n")
    (let ((to-be-removed '()))
      ;; remove all those
      (if (table-ref table under-symbol)
	  (setq to-be-removed
		(weak-ref (table-ref table under-symbol))))
      (when to-be-removed
	;; delete-from-list
	(mapcar
	 (lambda (fun)
	   (remove-function list-symbol fun))
	 (list to-be-removed)))
      (table-set table under-symbol
		 (make-weak-ref item)))

    (DB "add-to-list-under-symbol: solved the table\n")
    ;; now add
    (if at-end
	(set list-symbol (nconc (symbol-value list-symbol) (cons item nil)))
      (set list-symbol (cons item (symbol-value list-symbol)))))


  (define (list-hook-symbols symbol)
    "get a list of symbols (features) under which some hook function is `registered' in the variable given by symbol."
    (let ((hash (get symbol 'hook-mapping))
	  (symbols '()))
      (if (and hash
	       ;; (hash-p hash)
	       )
      (table-walk (lambda (key value)
		    (declare (unused value))
		    (format #t "%s\n" key)
		    (setq symbols
			  (cons key symbols)))
		      hash))
      symbols))


;;; Hook manipulation
  (define (mm-add-hook hook-symbol new-func #!optional at-end under-symbol)
    "Arrange it so that FUNCTION-NAME is added to the hook-list stored in
symbol, HOOK-SYMBOL. It will added at the head of the list unless AT-END
is true in which case it is added at the end."
    (DB "mm-add-hook: %s %s\n"  hook-symbol under-symbol)
    (add-to-list-under-symbol hook-symbol new-func at-end under-symbol 'hook-mapping remove-hook))


  ;; the only difference is the `order' or args !!
  (define (add-hook-under-symbol hook-symbol new-func under-symbol #!optional at-end)
    "Arrange it so that FUNCTION-NAME is added to the hook-list stored in
symbol, HOOK-SYMBOL. It will added at the head of the list unless AT-END
is true in which case it is added at the end."
    (add-to-list-under-symbol hook-symbol new-func at-end under-symbol 'hook-mapping remove-hook))


  ;;; transition macro:  use the function (given as symbol) as the TAG
  (defmacro add-hook-s (hook-variable function)	;#!optional at-end
    ;; mmc: hook-variable should not be unquoted.
    `(mm-add-hook ,hook-variable ,function #f (quote ,function))) ;at-end



  (define (mm-remove-hook hook-symbol old-func)
    "Remove FUNCTION-NAME from the hook HOOK-SYMBOL."
    (set hook-symbol (delete old-func (symbol-value hook-symbol)))

    ;; now, remove from the table:
    (let ((hash (get hook-symbol 'hook-mapping)))
      (if hash
	  ;;
	  (table-walk
	   (lambda (key value)
	     (DB "%s-> %s\n" key value)
	     (if (eq value old-func)
		 (table-set hash key #f)))
	   hash))))

  (define (mm-hook-under-symbol hook-symbol key)
    "return function from the hook HOOK-SYMBOL associated w/  KEY."
    ;; and-let*
    (let ((hash (get hook-symbol 'hook-mapping)))
      (if hash
	  ;;
	  (let ((function (table-ref hash key)))
	    (if (and function
		     (weak-ref function))
		(member (weak-ref function) (symbol-value hook-symbol)))))))


;; now, remove from the table:
  (define (mm-remove-hook-symbol hook-symbol key)
    "Remove from HOOK (given by hook-symbol) the function, which was added under the symbol `key'"
    (let ((hash (get hook-symbol 'hook-mapping)))
      (when hash
	(let ((function (table-ref hash key)))
	  (when (and function
		     (weak-ref function))
					;(if debug (message (format nil "%s-> %s\n" key value)))
	    (set hook-symbol (delete (weak-ref function) (symbol-value hook-symbol)))
	    (table-set hash key #f))))))

  (define (remove-hook-under-symbol hook-symbol symbol)
    "Remove from HOOK-SYMBOL the function, which was added under the symbol `SYMBOL'"
    (mm-remove-hook-symbol hook-symbol symbol))
  )
