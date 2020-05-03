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

DATA is the data to store, usually a symbol, never 'list'"

   ;; We could make a dedicated symbol instead of `list', but it
   ;; hasn't presented a problem yet.  Or just indicate listness with
   ;; t or nil.
   (let* 
      (  (tree (or tree '(list)))
	 (subtree-info-list '())
	 ;; A list* of tag, 'list, and current alist being explored
	 (current-tag+tree (cons nil tree)))

      ;; Drill down to the component that we want.
      (dolist (subname subname-list)
	 (if (eq (cadr current-tag+tree) 'list)
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
			      'list
			      (delete found alist))
			   subtree-info-list))
		     (setq current-tag+tree found))
	       
		  (progn
		     (setq subtree-info-list
			(cons
			   current-tag+tree
			   subtree-info-list))
		     (setq current-tag+tree (cons subname '(list))))))

	    ;; What we're exploring is a structure.  Demote it to the
	    ;; next level and make a list.
	    (progn
	       (setq subtree-info-list
		  (cons
		     (list*
			(car current-tag+tree)
			'list
			;; One sub-element, same as old except with an
			;; empty subname
			(list
			   (cons "" (cdr current-tag+tree))))
		     subtree-info-list))
	       (setq current-tag+tree (cons subname '(list))))))

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
	    (eq (cadr current-tag+tree) 'list)
	    (not (null (cddr current-tag+tree))))
	 (setq subtree-info-list
	    (cons
	       current-tag+tree
	       subtree-info-list))
	 (setq current-tag+tree (cons "" '(unbuilt))))
      
      ;; Now current-tag+tree points at the leaf that corresponds to
      ;; subname-list

      ;; Put the data in place.  This works but data must not be the
      ;; symbol 'list'
      (setcdr current-tag+tree (list data))

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


(defun firrtl-dbg-add-ephemeral (full-name value valid-p)
   ""
   (let* 
      ((data
	  (make-firrtl-ephemeral
	     :current (make-component-value :v value :valid-p valid-p)
	     :full-name full-name))
	 (sym (intern full-name firrtl-dbg-obarray)))
      (set sym data)
      (firrtl-mutate-current-components full-name sym)))




;; Helper to set up current data to develop on
(defun firrtl-write-to-current-components (comp)
   ""
   (firrtl-mutate-current-components
      (firrtl-component-full-name comp)
      comp))

;; Set up some current data to develop on
'
(progn
    (firrtl-write-to-current-components
       (make-firrtl-ephemeral
	  :current (make-component-value :v 15)
	  :full-name "a.b"))
    (firrtl-write-to-current-components
       (make-firrtl-ephemeral
	  :current (make-component-value :v 14)
	  :full-name "a.c")))

;; BUG about structure accesses, but just because proper type
;; management isn't ready yet.
'
(mapcar #'firrtl-write-to-current-components comp-list)


;; Setup:
;; Check that we're in sbt
;; Load anything local

;; WORK IN PROGRESS

;; But instead use buffer-substring-no-properties to capture it.
(setq firrtl-state-string (car kill-ring))
;; Te remove properties
(setq firrtl-state-string (substring-no-properties firrtl-state-string))

(setq spl (split-string firrtl-state-string "\n"))

(setq circuit-state-str (car spl)) ;; "CircuitState 2 (FRESH)"

(let* 
   ((str circuit-state-str)
      (m (string-match "CircuitState \\([0-9]+\\) (\\([A-Z]+\\))" str))
      (step (string-to-number (match-string 1 str)))
      ;; We need better info on this.  Only "FRESH" or "STALE"?
      (freshness-str (match-string 2 str)))
   
   (setq firrtl-current-circuit-freshness-str freshness-str)
   (setq firrtl-current-step step))

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
   (firrtl-dbg-split-input-line (sixth spl) "Ephemera: *"))

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
(defun firrtl-dbg-insert-ephemeral-component (wid)
   "Insert an ephemeral component"
   (let* 
      (
	 ;; (v0 (widget-get wid :value))
	 ;; (full-name (firrtl-ephemeral-full-name v0))
	 ;; ;; Later we'll have this directly as :value
	 ;; (sym (intern full-name firrtl-dbg-obarray))
	 (sym (widget-get wid :value))
	 (v (symbol-value sym))
	 (val (firrtl-ephemeral-current v))
	 (val-string (number-to-string (component-value-v val))))
      

      (widget-insert (firrtl-ephemeral-full-name v))
      ;; `current-column' seems to take visual widget indentation into
      ;; account, so no extra work is needed for this.
      (while (< (current-column) firrtl-dbg-value-column)
	 (widget-insert " "))

      (case (component-value-valid-p val)
	 ((t) )
	 ((nil)
	    (setq val-string
	       (propertize val-string 'face 'firrtl-dbg-face-invalid))))
      
      (widget-insert val-string)))

(defun firrtl-dbg-tree-widget (cell)
   (let ()
      (if (eq (second cell) 'list)
	 `(tree-widget
	     :node (push-button
		      :value ,(cddr cell)
		      :tag ,(car cell)
		      :format "%[%t%]\n"
		      :notify firrtl-punt-notify)
	     :dynargs firrtl-dbg-tree-expand)
	 (let*
	    ((sym (cadr cell)))
	    `(const
		:format "%v\n"
		:value ,sym
		:value-create ,#'firrtl-dbg-insert-ephemeral-component)))))


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


;; `widget-browse-at' gives us current widgets.  It looks at text
;; properties.  The one of interest is `button' because every
;; component's widget has a button.  

;; So we could just look at all the text properties and update the
;; relevant widgets.  Buffer-last ones first, to be efficient.  Can use next-single-property-change or next-property-change

;;;_. Footers
;;;_ , Provides

(provide 'firrtl-debugger)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: emacs-lisp
;;;_  + End:

;;;_ , End
;;; firrtl-debugger.el ends here
