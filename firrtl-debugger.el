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

;; Maybe valid-p can take a n/a value as well.  Maybe add a timestamp.
(defstruct (firrtl-dbg-value (:type list))
   ""
   v
   valid-p)

(defstruct (firrtl-dbg-component (:type list) :named)
   "The base of FIRRTL component info for widgets"
   full-name
   current ;; A firrtl-dbg-value
   )

(defstruct (firrtl-dbg-register
	      (:type list)
	      (:include firrtl-dbg-component)
	      :named)
   "A register"
   next ;; A firrtl-dbg-value
   )

(defstruct (firrtl-dbg-ephemeral
	      (:type list)
	      (:include firrtl-dbg-component)
	      :named)
   "A wire")

(defstruct (firrtl-dbg-input
	      (:type list)
	      (:include firrtl-dbg-component)
	      :named)
   "An input wire"
   user-input)


(defstruct (firrtl-dbg-output
	      (:type list)
	      (:include firrtl-dbg-component)
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

(defvar firrtl-dbg-have-built-subname-tree
   nil
   "True if we have already built the subname tree" )

(defvar firrtl-dbg-current-step
   0
   "The current step of the circuit" )

(defvar firrtl-dbg-current-freshness
   "UNKNOWN"
   "The current freshness of the circuit, as a string" )

(defvar firrtl-dbg-subname-tree
   '()
   "The component-tree of the circuit.

Format: Each node is either:
  (subname-string t list-of-nodes)
  (subname-string nil . sym)

")


(defun firrtl-dbg-add-to-subname-tree (tree subname-list data)
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
(firrtl-dbg-add-to-subname-tree '() '("a" "b")
   'my-data)

'
(firrtl-dbg-add-to-subname-tree '(list ("a" list ("b" my-data))) '("a" "b")
    'new-data)

'
(firrtl-dbg-add-to-subname-tree '(list ("a" list ("b" my-data))) '("a" "c")
    'new-data)

'
(firrtl-dbg-add-to-subname-tree '(list ("a" list ("b" my-data))) '("d" "b")
    'new-data)


(defun firrtl-dbg-split-component-name (str)
   ""
   (split-string str "[._]+"))
'
(firrtl-dbg-split-component-name "io_a.b")

(defun firrtl-dbg-mutate-subname-tree (full-name data)
   ""
   (setq
      firrtl-dbg-subname-tree
      (firrtl-dbg-add-to-subname-tree firrtl-dbg-subname-tree
	 (firrtl-dbg-split-component-name full-name)
	 data)))

(defun firrtl-dbg-add-object (full-name proc-mutate proc-create)
   ""
   (let* 
      (
	 (soft-sym (intern-soft full-name firrtl-dbg-obarray))
	 (sym (or soft-sym (intern full-name firrtl-dbg-obarray))))
      (if soft-sym
	 (funcall proc-mutate (symbol-value sym))
	 ;; Since it doesn't exist, create it
	 (set sym (funcall proc-create)))
      
      (when (not firrtl-dbg-have-built-subname-tree)
	 (firrtl-dbg-mutate-subname-tree full-name sym))))


(defun firrtl-dbg-add-ephemeral (full-name value valid-p)
   ""
   (firrtl-dbg-add-object full-name
      ;; Later, we'll check equality and set a timestamp.
      #'(lambda (object)
	   (setf (firrtl-dbg-ephemeral-current object)
	      (make-firrtl-dbg-value :v value :valid-p valid-p)))
      #'(lambda ()
	   (make-firrtl-dbg-ephemeral
	       :current (make-firrtl-dbg-value :v value :valid-p valid-p)
	      :full-name full-name))))

(defun firrtl-dbg-add-input (full-name value valid-p)
   ""
   (firrtl-dbg-add-object full-name
      ;; Later, we'll check equality and set a timestamp.
      #'(lambda (object)
	   (setf (firrtl-dbg-input-current object)
	      (make-firrtl-dbg-value :v value :valid-p valid-p)))
      #'(lambda ()
	   (make-firrtl-dbg-input
	       :current (make-firrtl-dbg-value :v value :valid-p valid-p)
	      :full-name full-name))))


(defun firrtl-dbg-add-output (full-name value valid-p)
   ""
   (firrtl-dbg-add-object full-name
      ;; Later, we'll check equality and set a timestamp.
      #'(lambda (object)
	   (setf (firrtl-dbg-output-current object)
	      (make-firrtl-dbg-value :v value :valid-p valid-p)))
      #'(lambda ()
	   (make-firrtl-dbg-output
	       :current (make-firrtl-dbg-value :v value :valid-p valid-p)
	      :full-name full-name))))

(defun firrtl-dbg-set-register-current (full-name value valid-p)
   ""

   (firrtl-dbg-add-object full-name
      ;; Later, we'll check equality and set a timestamp.
      #'(lambda (object)
	   (setf (firrtl-dbg-register-current object)
	      (make-firrtl-dbg-value :v value :valid-p valid-p)))
      #'(lambda ()
	   (make-firrtl-dbg-register
	      :current (make-firrtl-dbg-value :v value :valid-p valid-p)
	      :full-name full-name))))

(defun firrtl-dbg-set-register-next (full-name value valid-p)
   ""
   
   (firrtl-dbg-add-object full-name
      ;; Later, we'll check equality and set a timestamp.
      #'(lambda (object)
	   (setf (firrtl-dbg-register-next object)
	      (make-firrtl-dbg-value :v value :valid-p valid-p)))
      #'(lambda ()
	   (make-firrtl-dbg-register
	      :next (make-firrtl-dbg-value :v value :valid-p valid-p)
	      :full-name full-name))))


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
   
      (setq firrtl-dbg-current-freshness freshness-str)
      (setq firrtl-dbg-current-step step)))



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

;; '
;; (firrtl-dbg-act-on-component-str (second split) #'list)
;; '
;; (firrtl-dbg-act-on-component-str (third ephems)
;;    #'firrtl-dbg-add-ephemeral)
;; '
;; (mapcar
;;    #'(lambda (v)
;; 	(firrtl-dbg-act-on-component-str v #'firrtl-dbg-add-ephemeral))
;;    ephems)

(defun firrtl-dbg-build-data (state-string)
   ""

   (let
      ((spl (split-string state-string "\n")))

      ;; We do nothing with step data yet
      (while (string-match "^step [0-9]+" (car spl))
	 (setq spl (cdr spl)))

      ;; Skip blank lines just before the data
      (while (string-match "^[ \t]*$" (car spl))
	 (setq spl (cdr spl)))
      
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
	      (firrtl-dbg-act-on-component-str v
		 #'firrtl-dbg-set-register-current))
	 (firrtl-dbg-split-input-line
	    (firrtl-dbg-state-strings-registers spl)
	    "Registers *: *"))

      (mapcar
	 #'(lambda (v)
	      (firrtl-dbg-act-on-component-str v
		 #'firrtl-dbg-set-register-next))
	 (firrtl-dbg-split-input-line
	    (firrtl-dbg-state-strings-future-registers spl)
	    "FutureRegisters: *"))

      (mapcar
	 #'(lambda (v)
	      (firrtl-dbg-act-on-component-str v #'firrtl-dbg-add-ephemeral))
	 (firrtl-dbg-split-input-line
	    (firrtl-dbg-state-strings-ephemera spl)
	    "Ephemera: *"))

      (when (not firrtl-dbg-have-built-subname-tree)
	 ;; LATER: Sort the newly built subname tree
	 )

      (setq firrtl-dbg-have-built-subname-tree t)))

;; Demo
;; '(firrtl-dbg-build-data firrtl-state-string)

(defun firrtl-dbg-clear ()
   "Clear all the values; ready to start again"
   
   (interactive)
   (setq firrtl-dbg-have-built-subname-tree nil)
   (setq firrtl-dbg-subname-tree '())
   (setq firrtl-dbg-obarray
      (make-vector firrtl-dbg-obarray-default-size nil)))



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
(defun firrtl-dbg-punt-notify (but &rest ignore)
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
      ((space (propertize " " 'face face)))
      (widget-insert space) ;; Force at least one space
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
	  (if (firrtl-dbg-value-valid-p cvalue)
	     nil
	     'firrtl-dbg-face-invalid)))
      
      (list
	 (number-to-string
	    (firrtl-dbg-value-v cvalue))
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
	    (list (firrtl-dbg-ephemeral-full-name v) nil firrtl-dbg-value-column)
	    (firrtl-dbg-field-fmt
	       (firrtl-dbg-ephemeral-current v)
	       firrtl-dbg-value-end-column)))))


;; TEMPORARY clone.  This will get an input field
(defun firrtl-dbg-insert-input-component (wid)
   "Insert an input component"
   (let* 
      (
	 (sym (widget-get wid :value))
	 (v (symbol-value sym))
	 (val (firrtl-dbg-input-current v))
	 (val-string (number-to-string (firrtl-dbg-value-v val)))
	 (val-face
	    (if (firrtl-dbg-value-valid-p val)
	       nil
	       'firrtl-dbg-face-invalid))
	 from to
	 )
      
      (firrtl-dbg-insert-fields
	 (list
	    (list (firrtl-dbg-input-full-name v) nil firrtl-dbg-value-column)
	    (list val-string val-face firrtl-dbg-value-end-column)))
      (setq from (point))

      ;; Make a field explicitly
      '(widget-create-child-and-convert
	 wid 'integer
	 :tag " ABC ")
      ;; Maybe do this semi-manually?
      '(widget-setup)
      ;; Problem: Re-folding doesn't eliminate this field, so we get
      ;; overlapping fields.
      
      ;; widget-field-value-create ?  Does some stuff for us like
      ;; setting widget-field-new, but we want to handle that
      ;; manually.  And we need to call widget-field-value-delete when
      ;; we fold it.
      ;;(widget-insert "abcdefgh")
      ;; 2 so that rear-stickiness doesn't make later printing inherit
      ;; this.
      ;;(setq to (- (point) 2))
      ;;(widget-specify-field wid from to)
      ))


(defun firrtl-dbg-insert-output-component (wid)
   "Insert an output component"
   (let* 
      (
	 (sym (widget-get wid :value))
	 (v (symbol-value sym)))
      
      (firrtl-dbg-insert-fields
	 (list
	    (list (firrtl-dbg-output-full-name v) nil firrtl-dbg-value-column)
	    (firrtl-dbg-field-fmt
	       (firrtl-dbg-output-current v)
	       firrtl-dbg-value-end-column)))))

(defun firrtl-dbg-insert-register-component (wid)
   "Insert a register component"
   (let* 
      (
	 (sym (widget-get wid :value))
	 (v (symbol-value sym)))
      
      (firrtl-dbg-insert-fields
	 (list
	    (list (firrtl-dbg-register-full-name v) nil firrtl-dbg-value-column)
	    (firrtl-dbg-field-fmt
	       (firrtl-dbg-register-current v)
	       firrtl-dbg-value-end-column)
	    (list " -> " nil firrtl-dbg-next-value-begin-column)
	    (firrtl-dbg-field-fmt
	       (firrtl-dbg-register-current v)
	       firrtl-dbg-next-value-end-column)))))



(defun firrtl-dbg-tree-widget (cell)
   (let ()
      (if (second cell)
	 `(tree-widget
	     :node (push-button
		      :value ,(cddr cell)
		      :tag ,(car cell)
		      :format "%[%t%]\n"
		      :notify firrtl-dbg-punt-notify)
	     :dynargs firrtl-dbg-tree-expand)
	 (let*
	    ((sym (cddr cell)))

	    (etypecase (symbol-value sym)
	       (firrtl-dbg-ephemeral
		  `(const
		      :format "%v\n"
		      :value ,sym
		      :value-create ,#'firrtl-dbg-insert-ephemeral-component))
	       (firrtl-dbg-input
		  `(const
		      :format "%v\n"
		      :value ,sym
		      :value-create ,#'firrtl-dbg-insert-input-component
		      :notify
		      ,#'firrtl-dbg-do-integer-edit&poke))
	       
	       (firrtl-dbg-output
		  `(const
		      :format "%v\n"
		      :value ,sym
		      :value-create ,#'firrtl-dbg-insert-output-component))
	       (firrtl-dbg-register
		  `(const
		      :format "%v\n"
		      :value ,sym
		      :value-create
		      ,#'firrtl-dbg-insert-register-component)))))))

(defun firrtl-dbg-tree-expand (tree)
   (or (widget-get tree :args)
      (let
	 ((alist (widget-get (tree-widget-node tree) :value)))
	 (mapcar #'firrtl-dbg-tree-widget alist))))

(defun firrtl-dbg-create-widgets ()
   (widget-insert "Preliminary:  FIRRTL debugger interface\n\n")
   (widget-apply-action
      (widget-create (firrtl-dbg-tree-widget
			(cons "root" firrtl-dbg-subname-tree))))
   ;; widget-setup is not sufficient.  Unexpanded entries don't become
   ;; editable.  They do get put into widget-field-new tho.  Mostly
   ;; setup calls widget-specify-field and zaps markers
   (widget-setup) 
   (if (require 'tree-mode nil t)
      (tree-minor-mode t)
      (widget-insert "\n\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Updating widgets due to new "show"

(defun firrtl-dbg-redraw-widgets ()
   ""

   ;; Look at the text properties to find the relevant widgets.
   ;; Update them.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun firrtl-dbg-read-new-val-text (component)
   ""
   
   (let*
      (	 
	 (old-val (firrtl-dbg-input-current component))
	 (component-name (firrtl-dbg-input-full-name component))

	 ;; User can quit, then this just pops back via error handling.

	 ;; This assumes signed decimal.  We'd also like "choice" for
	 ;; enums.  And binary, and hex, and distinguish signedness
	 ;; (string-match "^-?[0-9]+$" text)
	 (new-val (read-number
		     (format "New value for %s: " component-name)
		     (if
			(firrtl-dbg-value-valid-p old-val)
			(firrtl-dbg-value-v old-val)
			nil))))
      
      ;; PUNT: Using type info, check new-val for bit width and
      ;; signedness.  Abort if new-val is not conformant.

      ;; The FIRRTL REPL will accept huge numbers as text, but
      ;; then complain if it's too wide.

      (when nil
	 (error "Not a number: %S" new-val))
      (list new-val (number-to-string new-val))))

(defun firrtl-dbg-parse-response-maybe-complain (str)
   ""
   
   (let*
      (
	 (legit-rx (concat " *" firrtl-dbg-tq-prompt-string))
	 (start-legit (string-match legit-rx str)))
      ;; Show any error that we get back
      (when (or (null start-legit) (> start-legit 0))
	 (message "%s" (substring str 0 start-legit)))))


(defun firrtl-dbg-do-integer-edit&poke (widget widget-again &optional event)
   ""

   (let* 
      (  (sym (widget-get widget :value))
	 (component (symbol-value sym))
	 (component-name (firrtl-dbg-input-full-name component))
	 (current (firrtl-dbg-input-current component))
	 (new-data (firrtl-dbg-read-new-val-text component))
	 (new-val (first new-data))
	 (new-val-text (second new-data))
	 (msg (concat "poke " component-name " " new-val-text "\n")))
      
      ;; IMPROVE ME:  Pre-filter inputs so we don't get errors here.
      (tq-enqueue firrtl-dbg-tq
	 msg
	 firrtl-dbg-tq-regexp
	 nil
	 #'(lambda (data str)
	      (firrtl-dbg-parse-response-maybe-complain str)))
      

      ;; Set the component's value to that
      (setf (firrtl-dbg-value-v current) new-val)

      ;; IMPROVE ME: This should allow another value meaning "User has
      ;; set this"
      (setf (firrtl-dbg-value-valid-p current) t)

      ;; Cause the widget to be redrawn.
      (widget-value-set widget (widget-value widget))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-hash-table :test 'eq
;; Callback, add (sym new-val) to hash table.  (puthash)
;; Then use all of them (maphash)

;; Construct a string for each, concat the strings plus "step" and
;; "show", separated by ";"

;; Then clrhash, next time start fresh.


;; Make a dedicated buffer.
;; Provide customizations for directory (of buffer) and repl main name.
;; sbt 'test:runMain gcd.GCDRepl'
;; And prompt, default = "firrtl>> "
;; And process name, and buffer name.

(defconst firrtl-dbg-executable
   "sbt"
   "" )
;; This has to be customizable, and may be set explicitly in the entry point.
(defconst firrtl-dbg-working-directory
   "/home/tehom/projects/ic-fab/ChiselProjects/tryout-chisel/"
   "" )
(defconst firrtl-dbg-repl-launch-string
   "test:runMain gcd.GCDRepl"
   "" )
(defconst firrtl-dbg-process-name
   "firrtl-dbg-process"
   "" )
(defconst firrtl-dbg-process-buffer-name
   "*Firrtl-dbg process*"
   "" )

(defconst firrtl-dbg-widgets-buffer-name
   "*Firrtl-dbg circuit state*"
   "" )

(defconst firrtl-dbg-tq-prompt-string
   "firrtl>>"
   "" )

(defconst firrtl-dbg-tq-regexp
   (concat ".*" firrtl-dbg-tq-prompt-string " *")
   "" )

;; Process buffer and widget buffer are distinct
(defvar firrtl-dbg-process-buffer
   nil
   "" )

(defvar firrtl-dbg-widgets-buffer
   nil
   "" )

(defvar firrtl-dbg-process
   nil
   "" )

;;;;;;;;;;;;;;;;;;;;
;; Dev of widgets buffer overall
'
(setq firrtl-dbg-widgets-buffer
   (generate-new-buffer firrtl-dbg-widgets-buffer-name))

;; And pop-to-buffer
'
(with-current-buffer firrtl-dbg-widgets-buffer
   (firrtl-dbg-create-widgets))

;; For the updates
'
(with-current-buffer firrtl-dbg-widgets-buffer
   (firrtl-dbg-redraw-widgets))

;;;;;;;;;;;;;;;;;;;;

'
(setq firrtl-dbg-process-buffer
   (generate-new-buffer firrtl-dbg-process-buffer-name))

'
(with-current-buffer firrtl-dbg-process-buffer
   (setq default-directory firrtl-dbg-working-directory))

;; This didn't quite work.  Wrong directory?  Worked when
;; executed in that buffer.

'
(with-current-buffer firrtl-dbg-process-buffer
   (setq firrtl-dbg-process
      (start-process
	 firrtl-dbg-process-name
	 firrtl-dbg-process-buffer
	 firrtl-dbg-executable
	 ;; Quoting this string with shell-quote-argument actually messes
	 ;; us up.
	 firrtl-dbg-repl-launch-string)))


;; Wait for prompt; may be already there in buffer.
'
(setq firrtl-dbg-tq
   (tq-create firrtl-dbg-process))

'
(tq-enqueue firrtl-dbg-tq "show\n" firrtl-dbg-tq-regexp
   'ok
   #'(lambda (data str)
	(message str)
	(firrtl-dbg-build-data str)))

;; Working an example from the docs.  Works now.
'
(tq-enqueue firrtl-dbg-tq
   "poke io_value1 4;poke io_value2 4;poke io_loadingValues 1;show\n"
   firrtl-dbg-tq-regexp
   'ok
   #'(lambda (data str)
	(pop-to-buffer firrtl-dbg-widgets-buffer)
	(message str)
	(with-current-buffer firrtl-dbg-widgets-buffer
	   (firrtl-dbg-build-data str)
	   (firrtl-dbg-redraw-widgets))))

'
(tq-enqueue firrtl-dbg-tq
   "step ; show\n"
   firrtl-dbg-tq-regexp
   'ok
   #'(lambda (data str)
	(pop-to-buffer firrtl-dbg-widgets-buffer)
	(message str)
	(with-current-buffer firrtl-dbg-widgets-buffer
	   (firrtl-dbg-build-data str)
	   (firrtl-dbg-redraw-widgets))))


'
(tq-enqueue firrtl-dbg-tq
   "poke io_loadingValues 0;step;show\n"
   firrtl-dbg-tq-regexp
   'ok
   #'(lambda (data str)
	(pop-to-buffer firrtl-dbg-widgets-buffer)
	(message str)
	(with-current-buffer firrtl-dbg-widgets-buffer
	   (firrtl-dbg-build-data str)
	   (firrtl-dbg-redraw-widgets))))

'
(symbol-value (intern "io_loadingValues" firrtl-dbg-obarray))

;; Use this at the end.
'
(tq-close firrtl-dbg-tq)

;;;_. Footers
;;;_ , Provides

(provide 'firrtl-debugger)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: emacs-lisp
;;;_  + End:

;;;_ , End
;;; firrtl-debugger.el ends here
