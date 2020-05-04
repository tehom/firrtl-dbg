;;;_ firrtl-debugger.el --- FIRRTL debugger interface

;;;_. Headers
;;;_ , License
;; Copyright (C) 2020  Tom Breton (Tehom)

;; Author: Tom Breton (Tehom) <tehom@panix.com>
;; Keywords: hardware

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;_ , Commentary:

;;;_ , Requires

(require 'cl)
(require 'subr-x)
(require 'tree-widget)
;;;_. Body

;; Maybe valid-p can take a n/a value as well
(defstruct (component-value (:type list))
   ""
   v
   valid-p)

(defstruct (firrtl-component (:type list) :named)
   "The base of FIRRTL component info for widgets"
   full-name
   current ;; A component-value
   )

(defstruct (firrtl-register
	      (:type list)
	      (:include firrtl-component)
	      :named)
   "A register"
   next ;; A component-value
   )

(defstruct (firrtl-ephemeral
	      (:type list)
	      (:include firrtl-component)
	      :named)
   "A wire")

(defstruct (firrtl-input
	      (:type list)
	      (:include firrtl-component)
	      :named)
   "An input wire"
   user-input)


(defstruct (firrtl-output
	      (:type list)
	      (:include firrtl-component)
	      :named)
   "An output wire")

;; Make this customizable
(defface firrtl-dbg-face-invalid '((t :background "gray"))
   "The face for poisoned values")

;; WRITE ME:  Faces for normal values, just-changed values, etc


;; Local variables
;; MAKE ME LOCAL in the mode

(defconst firrtl-dbg-obarray-default-size 257 "" )

(defvar firrtl-dbg-obarray
   (make-vector firrtl-dbg-obarray-default-size nil)
   "Obarray that holds the data about FIRRTL components" )



(defvar firrtl-current-step
   0
   "The current step of the circuit" )

(defvar firrtl-current-circuit-freshness-str
   "UNKNOWN"
   "The current freshness of the circuit, as a string" )

(defvar firrtl-current-components
   '()
   "The component-tree of the circuit.

Format: Each node is either:
(subname-string 'list list-of-nodes)
(subname-string leaf . data)
Where 'leaf' is one of the node types.

")


(defun firrtl-write-to-component (tree subname-list data)
   "
TREE should be '(list subtree...) or '(tag values...) where tag is one of the component struct tags.

DATA is the data to store, usually a symbol"

   ;; We could make a dedicated symbol instead of `list', but it
   ;; hasn't presented a problem yet.  Or just indicate listness with
   ;; t or nil.
   (let* 
      (  (tree (or tree '(t)))
	 (subtree-info-list '())
	 ;; A list* of tag, t, and current alist being explored, or
	 ;; (tag nil . data)
	 (current-tag+tree (cons nil tree)))

      ;; Drill down to the component that we want.
      (dolist (subname subname-list)
	 (if (second current-tag+tree)
	    ;; What we're exploring is an alist
	    (let* 
	       (  (alist (cddr current-tag+tree))
		  (found
		     (assoc subname alist)))
	       (if found
		  (progn
		     (setq subtree-info-list
			(cons
			   (list*
			      (car current-tag+tree)
			      t
			      (delete found alist))
			   subtree-info-list))
		     (setq current-tag+tree found))
	       
		  (progn
		     (setq subtree-info-list
			(cons
			   current-tag+tree
			   subtree-info-list))
		     (setq current-tag+tree (list subname t)))))

	    ;; What we're exploring is a structure.  Demote it to the
	    ;; next level and make a list.
	    (progn
	       (setq subtree-info-list
		  (cons
		     (list*
			(car current-tag+tree)
			t
			;; One sub-element, same as old except with an
			;; empty subname
			(list
			   (cons "" (cdr current-tag+tree))))
		     subtree-info-list))
	       (setq current-tag+tree (list subname t)))))

      ;; If current-tag+tree points at a true list, drill once more
      ;; with a blank subname.  Note we might get a blank list because
      ;; the code above us assumes that current-tag+tree might be
      ;; partway thru a list of subnames, but here we know we've ended
      ;; the subnames.

      ;; After this, current-tag+tree had better represent a non-list,
      ;; because we can't put more than one distinct component into a
      ;; blank subname.
      (when
	 (and
	    (second current-tag+tree)
	    (not (null (cddr current-tag+tree))))
	 (setq subtree-info-list
	    (cons
	       current-tag+tree
	       subtree-info-list))
	 (setq current-tag+tree (list "" nil)))
      
      ;; Now current-tag+tree points at the leaf that corresponds to
      ;; subname-list

      ;; Put the data in place.
      (setcdr current-tag+tree (cons nil data))

      ;; Cons the subtrees back in.
      (dolist (old-tag+tree subtree-info-list)
	 (setq current-tag+tree
	    (list*
	       (car old-tag+tree)
	       (cadr old-tag+tree)
	       current-tag+tree
	       (cddr old-tag+tree))))
      
      (cdr current-tag+tree)))

'
(firrtl-write-to-component '() '("a" "b")
   'my-data)

'
(firrtl-write-to-component '(list ("a" list ("b" my-data))) '("a" "b")
    'new-data)

'
(firrtl-write-to-component '(list ("a" list ("b" my-data))) '("a" "c")
    'new-data)

'
(firrtl-write-to-component '(list ("a" list ("b" my-data))) '("d" "b")
    'new-data)


(defun firrtl-split-component-name (str)
   ""
   (split-string str "[._]+"))
'
(firrtl-split-component-name "io_a.b")

(defun firrtl-mutate-current-components (full-name data)
   ""
   (setq
      firrtl-current-components
      (firrtl-write-to-component firrtl-current-components
	 (firrtl-split-component-name full-name)
	 data)))

(defun firrtl-dbg-add-object (full-name proc-mutate proc-create)
   ""
   (let* 
      (
	 (soft-sym (intern-soft full-name firrtl-dbg-obarray))
	 (sym (or soft-sym (intern full-name firrtl-dbg-obarray))))
      (if soft-sym
	 (proc-mutate (symbol-value sym))
	 ;; Since it doesn't exist, create it
	 (set sym (proc-create)))
      
      (firrtl-mutate-current-components full-name sym)))


(defun firrtl-dbg-add-ephemeral (full-name value valid-p)
   ""
   (let* 
      (
	 (soft-sym (intern-soft full-name firrtl-dbg-obarray))
	 (sym (intern full-name firrtl-dbg-obarray)))
      (if soft-sym
	 ;; Later, we'll check equality and set a timestamp.
	 (setf (firrtl-ephemeral-current
		  (make-component-value :v value :valid-p valid-p)))
	 ;; Since it doesn't exist, create it
	 (set
	    sym
	    (make-firrtl-ephemeral
	       :current (make-component-value :v value :valid-p valid-p)
	       :full-name full-name)))
      
      (firrtl-mutate-current-components full-name sym)))

(defun firrtl-dbg-add-input (full-name value valid-p)
   ""
   (let* 
      (
	 (soft-sym (intern-soft full-name firrtl-dbg-obarray))
	 (sym (intern full-name firrtl-dbg-obarray)))
      (if soft-sym
	 ;; Later, we'll check equality and set a timestamp.
	 (setf (firrtl-input-current
		  (make-component-value :v value :valid-p valid-p)))
	 ;; Since it doesn't exist, create it
	 (set
	    (intern full-name firrtl-dbg-obarray)
	    (make-firrtl-input
	       :current (make-component-value :v value :valid-p valid-p)
	       :full-name full-name)))
      
      (firrtl-mutate-current-components full-name sym)))

(defun firrtl-dbg-add-output (full-name value valid-p)
   ""
   (let* 
      (
	 (soft-sym (intern-soft full-name firrtl-dbg-obarray))
	 (sym (intern full-name firrtl-dbg-obarray)))
      (if soft-sym
	 ;; Later, we'll check equality and set a timestamp.
	 (setf (firrtl-output-current
		  (make-component-value :v value :valid-p valid-p)))
	 ;; Since it doesn't exist, create it
	 (set
	    (intern full-name firrtl-dbg-obarray)
	    (make-firrtl-output
	       :current (make-component-value :v value :valid-p valid-p)
	       :full-name full-name)))
      
      (firrtl-mutate-current-components full-name sym)))

(defun firrtl-dbg-set-register-current (full-name value valid-p)
   ""
   
   (let*
      ()
      
      ))

(defun firrtl-dbg-set-register-next (full-name value valid-p)
   ""
   
   (let*
      ()
      
      ))

(defstruct (firrtl-dbg-state-strings (:type list))
   "Structuring the post-split strings"
   overview
   inputs
   outputs
   registers
   future-registers
   ephemera
   memories)




;; Setup:
;; Check that we're in sbt
;; Load anything local

;; WORK IN PROGRESS

;; But instead use buffer-substring-no-properties to capture it.
(setq firrtl-state-string (car kill-ring))
;; Te remove properties
(setq firrtl-state-string (substring-no-properties firrtl-state-string))

(setq spl (split-string firrtl-state-string "\n"))
;; Gives a firrtl-dbg-state-strings

(defun firrtl-dbg-read-overview (spl)
   ""
   (let* 
      ((str (firrtl-dbg-state-strings-overview spl))
	 (m (string-match "CircuitState \\([0-9]+\\) (\\([A-Z]+\\))" str))
	 (step (string-to-number (match-string 1 str)))
	 ;; We need better info on this.  Only "FRESH" or "STALE"?
	 (freshness-str (match-string 2 str)))
   
      (setq firrtl-current-circuit-freshness-str freshness-str)
      (setq firrtl-current-step step)))



;; Prefix rxes
;; "Inputs: *"
;; "Outputs: *"
;; "Registers *: *"
;; "FutureRegisters: *"
;; "Ephemera: *"
;; "Memories" ;; This one may be different.  None written yet.

(defun firrtl-dbg-split-input-line (input-str prefix-rx)
   ""
   (let* 
      (
	 (m (string-match prefix-rx input-str))
	 (end (match-end 0))
	 (input-str (substring input-str end)))
      (split-string input-str ",")))

'
(setq ephems
   (firrtl-dbg-split-input-line
      (firrtl-dbg-state-strings-ephemera spl) "Ephemera: *"))

(defun firrtl-dbg-act-on-component-str (component-str proc)
   "PROC should take three parms: name, value, and is-valid"
   (let* 
      (
	 (m (string-match
	       "^ *\\([^=]+\\)=\\(☠?\\) *\\([-0-9]+\\)\\(☠?\\)"
	       component-str))
	 (full-name (match-string 1 component-str))
	 (valid-p (string-empty-p (match-string 2 component-str)))
	 (value (string-to-number (match-string 3 component-str))))
      
      ;; 2 and 4 should be the same
      (assert (string-equal
		 (match-string 2 component-str)
		 (match-string 4 component-str) ))

      (funcall proc
	 full-name value valid-p)))

'
(firrtl-dbg-act-on-component-str (second split) #'list)
'
(firrtl-dbg-act-on-component-str (third ephems)
   #'firrtl-dbg-add-ephemeral)
'
(mapcar
   #'(lambda (v)
	(firrtl-dbg-act-on-component-str v #'firrtl-dbg-add-ephemeral))
   ephems)

'
(let
   ((spl (split-string firrtl-state-string "\n"))

      )
   
   '(defstruct (firrtl-dbg-state-strings (:type list))
       "Structuring the post-split strings"
       overview
       inputs
       outputs
       registers
       future-registers
       ephemera
       memories)
   
   (firrtl-dbg-read-overview spl)

   (mapcar
      #'(lambda (v)
	   (firrtl-dbg-act-on-component-str v #'firrtl-dbg-add-input))
      (firrtl-dbg-split-input-line
	 (firrtl-dbg-state-strings-inputs spl)
	 "Inputs: *"))
   (mapcar
      #'(lambda (v)
	   (firrtl-dbg-act-on-component-str v #'firrtl-dbg-add-output))
      (firrtl-dbg-split-input-line
	 (firrtl-dbg-state-strings-outputs spl)
	 "Outputs: *"))

   (mapcar
      #'(lambda (v)
	   (firrtl-dbg-act-on-component-str v #'firrtl-dbg-add-ephemeral))
      (firrtl-dbg-split-input-line
	 (firrtl-dbg-state-strings-ephemera spl)
	 "Ephemera: *"))
   )


;; firrtl-current-components


;; For most components, non-editable.  Just displays it.

;; For input edit, use 'integer.  Also shows name etc.  Must figure
;; out whether it has been changed.  But widgets let you add a
;; callback, so just tell a registry that it has changed (Maybe also
;; have a face for "You set this")
;;

;; Extras: 

;; Add buttons and shortcuts for the functionality (step&show, multistep, reset)
;; Just if it's input, insert a value modification widget.
;; Allow hex or bin values, and arbitrary user value conversions (enums)
;; Another buffer and structure defining enum value conversions?
;; Darker face if it's invalid (:value-face)
;; Sort them first: alphabetical and input/output/other
;; Add something to save a configuration (currently open/closed/value-format)


;; PLACEHOLDER
(defun firrtl-punt-notify (but &rest ignore)
   ""
   
   (let*
      ()
      
      ))

(defconst firrtl-dbg-value-column 15
   "Column that values should print at" )

(defconst firrtl-dbg-value-end-column 25
   "Column that values should end at" )

(defconst firrtl-dbg-next-value-begin-column
   (+ 4 firrtl-dbg-value-end-column)
   "Column that values should end at" )

(defconst firrtl-dbg-next-value-end-column
   (+ 20 firrtl-dbg-next-value-begin-column)
   "Column that next-value should end at" )


(defun firrtl-dbg-pad-to-column (column face)
   ""
   
   (let*
      ((space (propertize " " 'face face) ))
      (while (< (current-column) column)
	 (widget-insert space))))

(defun firrtl-dbg-insert-w-face (str face)
   ""
   
   (let*
      ((str (if face (propertize str 'face face) str)))

      (widget-insert str)))

(defun firrtl-dbg-insert-fields (field-list)
   "FIELD-LIST is a list of ((text face end-col) ...)

END-COL is the column to start the next field at.  Face is
applied up until that column."
   
   (dolist (field field-list)
      (destructuring-bind (text face end-col) field
	 (firrtl-dbg-insert-w-face text face)
	 (firrtl-dbg-pad-to-column end-col face))))


(defun firrtl-dbg-field-fmt (cvalue end-col)
   ""
   (let* 
      ((face
	  (if (component-value-valid-p cvalue)
	     nil
	     'firrtl-dbg-face-invalid)))
      
      (list
	 (number-to-string
	    (component-value-v cvalue))
	 face
	 end-col)))


(defun firrtl-dbg-insert-ephemeral-component (wid)
   "Insert an ephemeral component"
   (let* 
      (
	 (sym (widget-get wid :value))
	 (v (symbol-value sym)))

      (firrtl-dbg-insert-fields
	 (list
	    (list (firrtl-ephemeral-full-name v) nil firrtl-dbg-value-column)
	    (firrtl-dbg-field-fmt
	       (firrtl-ephemeral-current v)
	       firrtl-dbg-value-end-column)))))


;; TEMPORARY clone.  This will get an input field
(defun firrtl-dbg-insert-input-component (wid)
   "Insert an input component"
   (let* 
      (
	 (sym (widget-get wid :value))
	 (v (symbol-value sym))
	 (val (firrtl-input-current v))
	 (val-string (number-to-string (component-value-v val)))
	 (val-face
	    (if (component-value-valid-p val)
	       nil
	       'firrtl-dbg-face-invalid)))
      
      (firrtl-dbg-insert-fields
	 (list
	    (list (firrtl-input-full-name v) nil firrtl-dbg-value-column)
	    (list val-string val-face firrtl-dbg-value-end-column)))))


(defun firrtl-dbg-insert-output-component (wid)
   "Insert an output component"
   (let* 
      (
	 (sym (widget-get wid :value))
	 (v (symbol-value sym)))
      
      (firrtl-dbg-insert-fields
	 (list
	    (list (firrtl-output-full-name v) nil firrtl-dbg-value-column)
	    (firrtl-dbg-field-fmt
	       (firrtl-output-current v)
	       firrtl-dbg-value-end-column)))))

(defun firrtl-dbg-insert-register-component (wid)
   "Insert a register component"
   (let* 
      (
	 (sym (widget-get wid :value))
	 (v (symbol-value sym))
	 (val (firrtl-output-current v))
	 (val-string (number-to-string (component-value-v val)))
	 (val-face
	    (if (component-value-valid-p val)
	       nil
	       'firrtl-dbg-face-invalid)))
      
      (firrtl-dbg-insert-fields
	 (list
	    (list (firrtl-register-full-name v) nil firrtl-dbg-value-column)
	    (list val-string val-face firrtl-dbg-value-end-column)
	    (list " -> " nil firrtl-dbg-next-value-begin-column)
	    (firrtl-dbg-field-fmt
	       (firrtl-output-current v)
	       firrtl-dbg-next-value-end-column)))))



(defun firrtl-dbg-tree-widget (cell)
   (let ()
      (if (second cell)
	 `(tree-widget
	     :node (push-button
		      :value ,(cddr cell)
		      :tag ,(car cell)
		      :format "%[%t%]\n"
		      :notify firrtl-punt-notify)
	     :dynargs firrtl-dbg-tree-expand)
	 (let*
	    ((sym (cddr cell))
	       (v (message (format "%S -> %S" sym (symbol-value sym))))
	       (value-create-proc
		  (etypecase (symbol-value sym)
		     (firrtl-ephemeral
			#'firrtl-dbg-insert-ephemeral-component)
		     ;; PUNT
		     (firrtl-input
			#'firrtl-dbg-insert-input-component)
		     (firrtl-output
			#'firrtl-dbg-insert-output-component)
		     )))
	    
	    `(const
		:format "%v\n"
		:value ,sym
		:value-create ,value-create-proc)))))



(defun firrtl-dbg-tree-expand (tree)
   (or (widget-get tree :args)
      (let
	 ((alist (widget-get (tree-widget-node tree) :value)))
	 (mapcar #'firrtl-dbg-tree-widget alist))))

(defun firrtl-dbg-tree ()
   (widget-insert "Preliminary:  FIRRTL debugger interface\n\n")
   (widget-apply-action
      (widget-create (firrtl-dbg-tree-widget
			(cons "root" firrtl-current-components))))
   (if (require 'tree-mode nil t)
      (tree-minor-mode t)
      (widget-insert "\n\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Updating widgets due to new "show"


;; Look at the text properties to find the relevant widgets.  Update
;; them.

;; Find respective sym.  That's :node then :value, if that's a sym.
;; Then check timestamp, maybe do nothing.
;; Then redraw the node widget
(defun firrtl-dbg-redraw-widgets ()
   ""
   
   (let
      ((done nil)
	 (pos (point-min)))

      (while (not done)
	 (let* 
	    ((next-pos
		(next-single-char-property-change
		   pos 'button (current-buffer)))
	       (advanced
		  (and next-pos (not (equal next-pos (point-max))))))
	    (if advanced
	       (progn
		  (setq pos next-pos)
		  (let* 
		     ((maybe-widget (get-char-property next-pos 'button)))
		     (when maybe-widget
			;; WRITE ME: Check whether it's a leaf.  Check
			;; its timestamp to see whether we need to
			(let* 
			   ((widget (widget-get maybe-widget :node)))
			   (when (widget-get widget :value)
			      ;; This forces a redraw
			      (widget-value-set widget
				 (widget-value widget)))))))
	       (setq done t))))))



;; Dev help for changing values behind widgets' back
'
(symbol-value (intern "_GEN_3" firrtl-dbg-obarray))

'
(set (intern "_GEN_3" firrtl-dbg-obarray) '(firrtl-ephemeral "_GEN_3" (444 t)))

;;;_. Footers
;;;_ , Provides

(provide 'firrtl-debugger)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: emacs-lisp
;;;_  + End:

;;;_ , End
;;; firrtl-debugger.el ends here
