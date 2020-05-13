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
(require 'tq)
;;;_. Body

(defstruct (firrtl-dbg-value (:type list))
   ""
   v
   ;; 'state' is one of (ok poisoned set-by-user-now
   ;; set-by-user-earlier first-seen just-changed stayed-same) Some of
   ;; these aren't set yet, and 'ok' will yield to (first-seen
   ;; just-changed stayed-same)
   state
   ;; Not used yet
   last-time-changed)

(defstruct (firrtl-dbg-component-type (:type list))
   ""
   signed-p
   width)


(defstruct (firrtl-dbg-component (:type list) :named)
   "The base of FIRRTL component info for widgets"
   full-name
   current ;; A firrtl-dbg-value
   type ;; (un)signed, bit width, etc
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

(defstruct (firrtl-dbg-state-strings (:type list))
   "Structuring the post-split strings"
   overview
   inputs
   outputs
   registers
   future-registers
   ephemera
   memories)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations and constants

(defgroup firrtl-dbg nil "Customizations for Firrtl-dbg")

(defface firrtl-dbg-face-value-poison '((t :background "gray"))
   "The face for poisoned values"
   :group 'firrtl-dbg)

(defface firrtl-dbg-face-value-set-by-user-earlier
   '((t (:foreground "forest green")))
   "The face for values set earlier"
   :group 'firrtl-dbg)

(defface firrtl-dbg-face-value-set-by-user-now
   '((t (:background "LightCyan1")))
   "The face for values set since the last step"
   :group 'firrtl-dbg)

(defface firrtl-dbg-face-value-default '()
   "The face for normal values"
   :group 'firrtl-dbg)

;; It's tempting to make this buffer-local and save it in the working
;; directory.  But for now, it's simpler to let it be a normal
;; customization.
(defcustom firrtl-dbg-custom-enums
   '()
   "Customization for enumerated values"
   ;; The first string is the name of the enum.  The repeated strings
   ;; are the enumerated values.  We don't try to support arbitrary
   ;; starting points etc.
   :type '(repeat
	     (group string
		(repeat string)))
   :group 'firrtl-dbg)

(defconst firrtl-dbg-component-perm-spec
   '(choice
       (group
	  (const decimal))
       (group
	  (const boolean))
       (group
	  (const hexadecimal))
       (group
	  (const enum)
	  (string))
       (group
	  (const base)
	  (integer)))
   "Customize spec that applies to all components" )

(defconst firrtl-dbg-component-perm-standard-value
   '(decimal)
   "The standard value for components perm-spec")

(defvar firrtl-dbg-current-buffer-type nil
   "What type of buffer the current buffer is.  
Possible values are (nil 'main 'custom 'process).
Local in the relevant buffers." )

(defconst firrtl-dbg-obarray-default-size 257
   "The default size of an obarry" )

;;;;;;;;;;;;;;;;;;;;
;; History lists

(defcustom firrtl-dbg-directory-history
   '()
   "History list of working directories.  Put the directory that you would run sbt in for your project on this list.  Then 'firrtl-dbg-startup' will see it as a history item"
   :type '(repeat directory)
   :group 'firrtl-dbg)

(defcustom firrtl-dbg-repl-name-history
   '("test:runMain gcd.GCDRepl")
   "History list of FIRRTL REPL commands.  Put the full command that you would give sbt to run a FIRRTL debugger on this list.  Then 'firrtl-dbg-startup' will see it as a history item"
   :type '(repeat string)
   :group 'firrtl-dbg)

;;;;;;;;;;;;;;;;;;;;
;;Configuration

(defcustom firrtl-dbg-executable
   "sbt"
   "Name of the actual executable that helps launch the debugger REPL"
   :type 'string
   :group 'firrtl-dbg)


(defconst firrtl-dbg-process-name
   "firrtl-dbg-process"
   "Name for the process that communicates with the debugger REPL" )

(defconst firrtl-dbg-process-buffer-name
   "*Firrtl-dbg process*"
   "Name of the process buffer" )

;;;;;;;;;;;;;;;;;;;;
;; Regexps

(defcustom firrtl-dbg-tq-prompt-string
   "firrtl>>"
   "The REPL's prompt string"
   :type 'string
   :group 'firrtl-dbg)

(defconst firrtl-dbg-type-regexp
   "type \\([^ ]+\\) \\([0-9]+\\).\\([A-Z]+\\)<\\([0-9]+\\)>"
   "Regexp matching the string returned by the 'type' command" )

(defconst firrtl-dbg-tq-regexp
   (concat ".*" firrtl-dbg-tq-prompt-string " *")
   "Regexp matching any response from the REPL" )

;;;;;;;;;;;;;;;;;;;;
;; Print columns


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

(defconst firrtl-dbg-type-end-column
   (+ 20 firrtl-dbg-next-value-end-column)
   "Column that type should end at" )


;;;;;;;;;;;;;;;;;;;;

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


;; (firrtl-dbg-add-to-subname-tree '() '("a" "b")
;;    'my-data)


;; (firrtl-dbg-add-to-subname-tree '(list ("a" list ("b" my-data))) '("a" "b")
;;     'new-data)


;; (firrtl-dbg-add-to-subname-tree '(list ("a" list ("b" my-data))) '("a" "c")
;;     'new-data)


;; (firrtl-dbg-add-to-subname-tree '(list ("a" list ("b" my-data))) '("d" "b")
;;     'new-data)


(defun firrtl-dbg-split-component-name (str)
   ""
   (split-string str "[._]+"))

;; (firrtl-dbg-split-component-name "io_a.b")

(defun firrtl-dbg-mutate-subname-tree (full-name data)
   ""
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (firrtl-dbg-complain-bad-buffer
	 "Objects are only available in the main buffer"))

   (setq
      firrtl-dbg-subname-tree
      (firrtl-dbg-add-to-subname-tree firrtl-dbg-subname-tree
	 (firrtl-dbg-split-component-name full-name)
	 data)))

(defun firrtl-dbg-add-object (full-name proc-mutate proc-create)
   ""
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (firrtl-dbg-complain-bad-buffer
	 "Objects are only available in the main buffer"))

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


(defun firrtl-dbg-add-ephemeral (full-name value state)
   ""
   (firrtl-dbg-add-object full-name
      ;; Later, we'll check equality and set a timestamp.
      #'(lambda (object)
	   (setf (firrtl-dbg-ephemeral-current object)
	      (make-firrtl-dbg-value :v value :state state)))
      #'(lambda ()
	   (make-firrtl-dbg-ephemeral
	       :current (make-firrtl-dbg-value :v value :state state)
	      :full-name full-name))))

(defun firrtl-dbg-add-input (full-name value state)
   ""
   (firrtl-dbg-add-object full-name
      ;; Later, we'll check equality and set a timestamp.
      #'(lambda (object)
	   (setf (firrtl-dbg-input-current object)
	      (make-firrtl-dbg-value :v value :state state)))
      #'(lambda ()
	   (make-firrtl-dbg-input
	       :current (make-firrtl-dbg-value :v value :state state)
	      :full-name full-name))))


(defun firrtl-dbg-add-output (full-name value state)
   ""
   (firrtl-dbg-add-object full-name
      ;; Later, we'll check equality and set a timestamp.
      #'(lambda (object)
	   (setf (firrtl-dbg-output-current object)
	      (make-firrtl-dbg-value :v value :state state)))
      #'(lambda ()
	   (make-firrtl-dbg-output
	       :current (make-firrtl-dbg-value :v value :state state)
	      :full-name full-name))))

(defun firrtl-dbg-set-register-current (full-name value state)
   ""

   (firrtl-dbg-add-object full-name
      ;; Later, we'll check equality and set a timestamp.
      #'(lambda (object)
	   (setf (firrtl-dbg-register-current object)
	      (make-firrtl-dbg-value :v value :state state)))
      #'(lambda ()
	   (make-firrtl-dbg-register
	      :current (make-firrtl-dbg-value :v value :state state)
	      :full-name full-name))))

(defun firrtl-dbg-set-register-next (full-name value state)
   ""
   
   (firrtl-dbg-add-object full-name
      ;; IMPROVE ME: check equality with existing value and set a
      ;; timestamp if it changed.
      #'(lambda (object)
	   (setf (firrtl-dbg-register-next object)
	      (make-firrtl-dbg-value :v value :state state)))
      #'(lambda ()
	   (make-firrtl-dbg-register
	      :next (make-firrtl-dbg-value :v value :state state)
	      :full-name full-name))))


(defun firrtl-dbg-read-overview (spl)
   ""
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (firrtl-dbg-complain-bad-buffer))

   (let* 
      ((str (firrtl-dbg-state-strings-overview spl))
	 (m (string-match "CircuitState \\([0-9]+\\) (\\([A-Z]+\\))" str))
	 (step (string-to-number (match-string 1 str)))
	 (freshness-str (match-string 2 str)))
   
      (setq firrtl-dbg-current-freshness freshness-str)
      (setq firrtl-dbg-current-step step)))

(defun firrtl-dbg-split-input-line (input-str prefix-rx)
   ""
   (let* 
      (
	 (m (string-match prefix-rx input-str))
	 (end (match-end 0))
	 (input-str (substring input-str end)))
      (split-string input-str ",")))


(defun firrtl-dbg-act-on-component-str (component-str proc)
   "PROC should take three parms: name, value, and is-valid"
   (let* 
      (
	 (m (string-match
	       "^ *\\([^=]+\\)=\\(☠?\\) *\\([-0-9]+\\)\\(☠?\\)"
	       component-str))
	 (full-name (match-string 1 component-str))
	 (valid-p (string-empty-p (match-string 2 component-str)))
	 (state
	    (if
	       (string-empty-p (match-string 2 component-str))
	       'ok
	       'poisoned))
	 (value (string-to-number (match-string 3 component-str))))
      
      ;; strings 2 and 4 should be the same
      (assert (string-equal
		 (match-string 2 component-str)
		 (match-string 4 component-str) ))

      (funcall proc
	 full-name value state)))


(defun firrtl-dbg-build-data (state-string)
   ""
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (firrtl-dbg-complain-bad-buffer
	 "Building the data only makes sense in a circuit buffer"))

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
	 ;; IMPROVE ME: Sort the newly built subname tree
	 )

      (setq firrtl-dbg-have-built-subname-tree t)))


(defun firrtl-dbg-clear ()
   "Clear all the values; ready to start again"
   (interactive)

   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (firrtl-dbg-complain-bad-buffer
	 "Clearing the data only makes sense in a circuit buffer"))

   (setq firrtl-dbg-have-built-subname-tree nil)
   (setq firrtl-dbg-subname-tree '())
   (setq firrtl-dbg-obarray
      (make-vector firrtl-dbg-obarray-default-size nil)))

(defun firrtl-dbg-shutdown ()
   ""
   
   (interactive)

   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (firrtl-dbg-complain-bad-buffer
	 "Shutting down only makes sense in a circuit buffer"))

   (firrtl-dbg-clear)
   
   (when firrtl-dbg-tq
      (tq-close firrtl-dbg-tq))
   (when firrtl-dbg-process-buffer
      (kill-buffer firrtl-dbg-process-buffer))
   (setq firrtl-dbg-process-buffer nil)
   (kill-buffer (current-buffer)))

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
      (when field
	 (destructuring-bind (text face end-col) field
	    (firrtl-dbg-insert-w-face text face)
	    (firrtl-dbg-pad-to-column end-col face)))))

(defun firrtl-dbg-get-face-by-validity (validity)
   ""
   
   (case validity
      (poisoned 'firrtl-dbg-face-value-poison)
      (set-by-user-now 'firrtl-dbg-face-value-set-by-user-now)
      (set-by-user-earlier 'firrtl-dbg-face-value-set-by-user-earlier)
      (ok 'firrtl-dbg-face-value-default)
      (t nil)))

(defun firrtl-dbg-enum-string (fmt index)
   ""
   
   (let*
      (  (key (second fmt))
	 (found (assoc key firrtl-dbg-custom-enums)))
      (if found
	 (let* 
	    ((strings (second found)))
	    (if (< index (length strings))
	       (nth index strings)
	       "[invalid]"))
	 "[no such enum]")))


(defun firrtl-dbg-field-fmt (cvalue perm-props end-col)
   ""
   (let* 
      ((face
	  (firrtl-dbg-get-face-by-validity
	     (firrtl-dbg-value-state cvalue)))
	 (fmt perm-props)
	 (text
	    (case (first fmt)
	       ((boolean)
		  ;; ENCAP ME
		  (case (firrtl-dbg-value-v cvalue)
		     (0 "false")
		     (1 "true")
		     (otherwise "[invalid]")))
	       ((enum)
		  (firrtl-dbg-enum-string fmt (firrtl-dbg-value-v cvalue)))
	       
	       (otherwise
		  (number-to-string
		     (firrtl-dbg-value-v cvalue))))))

      (list
	 text
	 face
	 end-col)))

(defun firrtl-dbg-type-fmt (component end-col)
   ""
   
   (let*
      ((type (firrtl-dbg-component-type component)))
      (if (null type)
	 (list "??" nil end-col)
	 (let* 
	    ((signed-str
		(if (firrtl-dbg-component-type-signed-p type)
		   "SInt"
		   "UInt"))
	       (width-str
		  (number-to-string
		     (firrtl-dbg-component-type-width type)))
	       (str
		  (concat signed-str "(" width-str ")")))
	 
	    (list str nil end-col)))))

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
	       (firrtl-dbg-get-perm-props (symbol-name sym))
	       firrtl-dbg-value-end-column)
	    (firrtl-dbg-type-fmt
	       v
	       firrtl-dbg-type-end-column)))))

(defun firrtl-dbg-insert-input-component (wid)
   "Insert an input component"
   (let* 
      (
	 (sym (widget-get wid :value))
	 (v (symbol-value sym)))

      (firrtl-dbg-insert-fields
	 (list
	    (list (firrtl-dbg-input-full-name v) nil firrtl-dbg-value-column)
	    (firrtl-dbg-field-fmt
	       (firrtl-dbg-input-current v)
	       (firrtl-dbg-get-perm-props (symbol-name sym))
	       firrtl-dbg-value-end-column)
	    (firrtl-dbg-type-fmt
	       v
	       firrtl-dbg-type-end-column)))))



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
	       (firrtl-dbg-get-perm-props (symbol-name sym))
	       firrtl-dbg-value-end-column)
	    (firrtl-dbg-type-fmt
	       v
	       firrtl-dbg-type-end-column)))))

(defun firrtl-dbg-insert-register-component (wid)
   "Insert a register component"
   (let* 
      (
	 (sym (widget-get wid :value))
	 (v (symbol-value sym))
	 (perm-props
	    (firrtl-dbg-get-perm-props (symbol-name sym))))
      
      (firrtl-dbg-insert-fields
	 (list
	    (list (firrtl-dbg-register-full-name v)
	       nil
	       firrtl-dbg-value-column)
	    (firrtl-dbg-field-fmt
	       (firrtl-dbg-register-current v)
	       perm-props
	       firrtl-dbg-value-end-column)
	    (list " -> " nil firrtl-dbg-next-value-begin-column)
	    (firrtl-dbg-field-fmt
	       (firrtl-dbg-register-next v)
	       perm-props
	       firrtl-dbg-next-value-end-column)
	    (firrtl-dbg-type-fmt
	       v
	       firrtl-dbg-type-end-column)))))

(defun firrtl-dbg-tree-widget (cell)
   (let ()
      (if (second cell)
	 `(tree-widget
	     :node (push-button
		      :value ,(cddr cell)
		      :tag ,(car cell)
		      :format "%[%t%]\n"
		      ;; Nothing to do yet for inner nodes
		      :alt-action ,#'ignore)
	     :dynargs firrtl-dbg-tree-expand)
	 (let*
	    ((sym (cddr cell)))

	    (etypecase (symbol-value sym)
	       (firrtl-dbg-ephemeral
		  `(const
		      :format "%v\n"
		      :value ,sym
		      :value-create ,#'firrtl-dbg-insert-ephemeral-component
		      :alt-action ,#'firrtl-dbg-edit-properties
		      ))
	       (firrtl-dbg-input
		  `(const
		      :format "%v\n"
		      :value ,sym
		      :value-create ,#'firrtl-dbg-insert-input-component
		      :notify
		      ,#'firrtl-dbg-do-integer-edit&poke
		      :alt-action ,#'firrtl-dbg-edit-properties
		      ))
	       
	       (firrtl-dbg-output
		  `(const
		      :format "%v\n"
		      :value ,sym
		      :value-create ,#'firrtl-dbg-insert-output-component
		      :alt-action ,#'firrtl-dbg-edit-properties
		      ))
	       (firrtl-dbg-register
		  `(const
		      :format "%v\n"
		      :value ,sym
		      :value-create
		      ,#'firrtl-dbg-insert-register-component
		      :alt-action ,#'firrtl-dbg-edit-properties
		      )))))))

(defun firrtl-dbg-tree-expand (tree)
   (or (widget-get tree :args)
      (let
	 ((alist (widget-get (tree-widget-node tree) :value)))
	 (mapcar #'firrtl-dbg-tree-widget alist))))

(defun firrtl-dbg-create-widgets ()
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (firrtl-dbg-complain-bad-buffer
	 "Creating the widgets only makes sense in a circuit buffer"))

   (widget-insert "FIRRTL debugger interface\n\n")

   (widget-create 'push-button
      :notify (lambda (&rest ignore)
		 (firrtl-dbg-step-circuit))
      "Step")

   (widget-insert "    ")

   (widget-create 'push-button
      :notify (lambda (&rest ignore)
		    (unless (eq firrtl-dbg-current-buffer-type 'main)
		       (firrtl-dbg-complain-bad-buffer
			  "Rebuilding the widgets only makes sense in a circuit buffer"))

		 (erase-buffer)
		 (firrtl-dbg-create-widgets))
      "Rebuild buffer")

   ;; IMPROVE ME: Add other buttons: Reset, Done, Poison, Randomize,
   ;; Start/stop recording script, etc
   (widget-insert "\n\n")

   (widget-apply-action
      (widget-create (firrtl-dbg-tree-widget
			(cons "root" firrtl-dbg-subname-tree))))
   (if (require 'tree-mode nil t)
      (tree-minor-mode t)
      (widget-insert "\n\n"))
   (use-local-map widget-keymap)
   (local-set-key "\M-\r"
      #'firrtl-dbg-do-alt-interaction))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun firrtl-dbg-edit-properties (widget &optional event)
   "Edit the properties of a component symbol"

   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (firrtl-dbg-complain-bad-buffer
	 "Calling edit-properties only makes sense in the main buffer, although it creates a customize buffer"))

   (let* 
      (
	 (sym (widget-get widget :value))
	 (name (symbol-name sym))
	 (perm-sym-soft
	    (intern-soft name firrtl-dbg-obarray-perm-props))
	 (perm-sym
	    (or perm-sym-soft
	       (intern name firrtl-dbg-obarray-perm-props))))

      (unless perm-sym-soft
	 (custom-declare-variable perm-sym
	    (list 'quote firrtl-dbg-component-perm-standard-value)
	    "The format to display the component in"
	    :type firrtl-dbg-component-perm-spec))

      (let
	 (  (main-buf (current-buffer))
	    (working-directory default-directory)
	    (buf
	     (custom-get-fresh-buffer
		(format "*Customize circuit component %s*"
		   (custom-unlispify-tag-name perm-sym)))))
	 (with-current-buffer buf
	    (setq default-directory working-directory)
	    (custom-buffer-create-internal
	       (list (list perm-sym 'custom-variable))
	       ;; The parm "description" doesn't do anything
	       nil)
	    (set
	       (make-local-variable 'firrtl-dbg-main-buffer)
	       main-buf)

	    (fset (make-local-variable 'Custom-save)
	       #'firrtl-dbg-save-perms)
	    (set (make-local-variable 'custom-variable-menu)
	       firrtl-dbg-custom-variable-menu))
	  
	 (pop-to-buffer-same-window buf))))


(defun firrtl-dbg-copy-alist-to-perms ()
   ""
   
   (interactive)
   (dolist (cell firrtl-dbg-perm-props-alist)
      (let* 
	 ((name (car cell))
	    (value (cdr cell))
	    (sym (intern name firrtl-dbg-obarray-perm-props)))
	 (set sym value))))



(defun firrtl-dbg-copy-perms-to-alist ()
   ""

   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (firrtl-dbg-complain-bad-buffer
	 "Copying to the perm alist only makes sense in the main buffer"))

   (setq firrtl-dbg-perm-props-alist '())
   (mapatoms
      #'(lambda (sym)
	   (when sym
	      (setq firrtl-dbg-perm-props-alist
		 (cons
		    (cons (symbol-name sym) (symbol-value sym))
		    firrtl-dbg-perm-props-alist))))
      firrtl-dbg-obarray-perm-props))


(defun firrtl-dbg-save-perms (&rest ignore)
   ""
   ;; This takes the place of Custom-save

   ;; CHECK ME: Do we need to copy perms to firrtl-dbg-perm-props-alist?

   ;; Quick&dirty: Just save firrtl-dbg-perm-props-alist.  We don't
   ;; save firrtl-dbg-custom-enums because that's useful globally
   ;; (though providing both local and global would be nice).  We
   ;; don't try to track what's dirty, nor treat an extensible set of
   ;; variables
   (add-dir-local-variable 'firrtl-dbg-mode
      'firrtl-dbg-perm-props-alist
      firrtl-dbg-perm-props-alist))


(defun firrtl-dbg-get-perm-props (str)
   "Get the permanent props for the component named STR.
Return nil if component has no permanent props."
   
   (let*
      (
	 (perm-prop-sym
	    (intern-soft str firrtl-dbg-obarray-perm-props)))
      (if perm-prop-sym (symbol-value perm-prop-sym) nil)))


(defun firrtl-dbg-custom-variable-save (widget)
   "Save value of variable edited by widget WIDGET."
   (custom-variable-mark-to-save widget)
   (save-excursion
      (let* 
	 ((sym (widget-get widget :value)))

	 ;; Customize buffer knows a particular widgets buffer
	 (with-current-buffer firrtl-dbg-main-buffer
	    (unless (eq firrtl-dbg-current-buffer-type 'main)
	       (firrtl-dbg-complain-bad-buffer))

	    ;; Copy this sym to firrtl-dbg-perm-props-alist
	    (setq firrtl-dbg-perm-props-alist
	       (cons
		  (cons (symbol-name sym) (symbol-value sym))
		  (delete-if
		     #'(lambda (a)
			  (string-equal (first a) (symbol-name sym)))
		     firrtl-dbg-perm-props-alist)))
	    ;; IMPROVE ME: Nice to save the file automatically and not
	    ;; necessarily see the buffer in a window.
	    (add-dir-local-variable 'firrtl-dbg-mode
	       'firrtl-dbg-perm-props-alist
	       firrtl-dbg-perm-props-alist))))
   
   (custom-variable-state-set-and-redraw widget))

(defun firrtl-dbg-make-custom-variable-menu ()
   ""
   
   (require 'cus-edit)
   (let*
      (
	 (our-menu '()))

      (dolist (i custom-variable-menu)
	 (let* 
	    ((entry
		(if (eq (second i) 'custom-variable-save)
		   '("Save for Future Sessions"
		       firrtl-dbg-custom-variable-save
		       (lambda
			  (widget)
			  (memq
			     (widget-get widget :custom-state)
			     '(modified set changed rogue))))
		   i)))
	    (setq our-menu (cons entry our-menu))))
      (nreverse our-menu)))

;; Needs to be after firrtl-dbg-make-custom-variable-menu
(defconst firrtl-dbg-custom-variable-menu
   (firrtl-dbg-make-custom-variable-menu)
   "" )

(defun firrtl-dbg-do-alt-interaction (pos &optional event)
   "Do the alternate widget interaction at pos"
   (interactive "@d")

   (let ((button (get-char-property pos 'button)))
      (if button
	 (let* 
	    ;; "button" is the parent widget, but we need the node.
	    ((widget (widget-get button :node)))
	    ;; Do we have to check :active as widget-apply-action does?

	    ;; Do :alt-action if it can
	    (when (widget-member widget :alt-action)
	       (widget-apply widget :alt-action event)))
	 
	 ;; Otherwise do whatever we'd have done in the global map
	 (let ((command (lookup-key widget-global-map (this-command-keys))))
	    (when (commandp command)
	       (call-interactively command))))))


(defun firrtl-dbg-step-circuit ()
   "Step the circuit"

   (when firrtl-dbg-writing-script-p
      (setq firrtl-dbg-current-script-rv
	 (cons '(step)
	    firrtl-dbg-current-script-rv)))
   
   (tq-enqueue firrtl-dbg-tq
      "step ; show\n"
      firrtl-dbg-tq-regexp
      (list (current-buffer))
      #'(lambda (data str)
	   (with-current-buffer (first data)
	      ;; IMPROVE ME: At some point change all states of
	      ;; set-by-user-now to set-by-user-earlier.  For
	      ;; non-inputs, figure out whether it changed since last
	      ;; time.
	      (firrtl-dbg-build-data str)
	      (firrtl-dbg-redraw-widgets)))))

(defun firrtl-dbg-remove-prompt-suffix (str)
   ""

   (let*
      (
	 (start-legit (string-match firrtl-dbg-tq-prompt-string str)))
      (when (null start-legit)
	 (error "No FIRRTL prompt found"))
      (substring str 0 start-legit)))

(defun firrtl-dbg-parse-component-type-string (str)
   ""
   (string-match firrtl-dbg-type-regexp str)
   (let*
      (	 (name (match-string 1 str))
	 (value (match-string 2 str))
	 (type-tag-str (match-string 3 str))
	 (width (string-to-number (match-string 4 str)))
	 (signed-p
	    (case type-tag-str
	       (("U" "PU") t)
	       (otherwise nil))))
      (make-firrtl-dbg-component-type
	 :signed-p signed-p
	 :width width)))


;; (firrtl-dbg-parse-component-type-string
;;    (firrtl-dbg-remove-prompt-suffix "type x 3.PU<4>
;; firrtl>> "))
;;     

(defun firrtl-dbg-init-component-type (name)
   "Set the type of component NAME according to the REPL"

   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (firrtl-dbg-complain-bad-buffer))
   
   (let*
      ((command (concat "type " name "\n")))
      (tq-enqueue firrtl-dbg-tq command firrtl-dbg-tq-regexp
	 (list (current-buffer) name)
	 #'(lambda (data str)
	      (with-current-buffer (first data)
		 (unless (eq firrtl-dbg-current-buffer-type 'main)
		    (firrtl-dbg-complain-bad-buffer))
		 (let* 
		    ((str (firrtl-dbg-remove-prompt-suffix str))
		       (type (firrtl-dbg-parse-component-type-string str))
		       (full-name (second data))
		       (sym (intern-soft full-name firrtl-dbg-obarray)))
		    (when sym
		       (let* 
			  ((component (symbol-value sym)))
			  (setf
			     (firrtl-dbg-component-type component)
			     type)))))))))

;; IMPROVE ME:  Do this automatically on startup.
(defun firrtl-dbg-init-all-component-types ()
   ""
   
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (firrtl-dbg-complain-bad-buffer))
   (mapatoms
      #'(lambda (sym)
	   (when sym
	      (firrtl-dbg-init-component-type (symbol-name sym))))
      firrtl-dbg-obarray))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Updating widgets due to new "show"

(defun firrtl-dbg-for-all-buttons (proc)
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
		     ((button (get-char-property next-pos 'button)))
		     (when button
			(funcall proc button))))
	       (setq done t))))))

(defun firrtl-dbg-redraw-widgets ()
   ""
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (firrtl-dbg-complain-bad-buffer))

   (firrtl-dbg-for-all-buttons
      #'(lambda (widget)
	   (let* 
	      ((widget (widget-get widget :node)))
	      (when (widget-get widget :value)
		 ;; This forces a redraw
		 (widget-value-set widget
		    (widget-value widget)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun firrtl-dbg-read-new-boolean-val (prompt old-val)
   "Return it as list of (number text)"

   (let*
      (  (default-string
	    ;; Reverse what was there, since there are only
	    ;; two possibilities
	    ;; bool number to string
	    (case (- 1 (firrtl-dbg-value-v old-val))
	       (0 "false")
	       (1 "true")
	       (otherwise nil)))
	 (new-string
	    (completing-read
	       prompt
	       '("true" "false")
	       nil t
	       default-string)))
      ;; bool string to number
      (cond 
	 ((string-equal new-string "true")
	    1)
	 ((string-equal new-string "false")
	    0)
	 (otherwise (error "Not a boolean")))))

(defun firrtl-dbg-read-new-decimal-val (prompt old-val)
   ""
   ;; IMPROVE ME: Using type info, check new-val for bit width and
   ;; signedness.  Abort if new-val is not conformant.
   (let ((new-val
	    (read-number
	       prompt
	       (if
		  (not
		     (eq
			(firrtl-dbg-value-state old-val)
			'poisoned))
		  (firrtl-dbg-value-v old-val)
		  nil))))
      new-val))

(defun firrtl-dbg-find-index-in-list (key str-list)
   "Return the index or nil"
   
   (let*
      ((index 0))

      (while (and
		str-list
		(not (equal key (car str-list))))
	 (incf index)
	 (setq str-list (cdr str-list)))
      ;; Normally return the index that we got to.  If there's nothing
      ;; left of str-list, we never found key so return nil.
      (if str-list
	 index
	 nil)))

;; Examples:
;; (firrtl-dbg-find-index-in-list "b" '(("b")("a")))
;; (firrtl-dbg-find-index-in-list "a" '(("b")("a")))

(defun firrtl-dbg-read-new-enum-val (prompt fmt)
   ""

   (let*
      (  (key (second fmt))
	 (found (assoc key firrtl-dbg-custom-enums)))
      (if found
	 (let* 
	    (  (strings (second found))
	       (new-string
		  (completing-read
		     prompt
		     strings
		     nil t
		     nil))
	       
	       (new-val
		  (firrtl-dbg-find-index-in-list new-string strings)))
	    
	    (if new-val
	       new-val
	       (error "Lost the enum string %s" new-string)))
	 
	 (error "No such enum: %s" key))))


(defun firrtl-dbg-read-new-val (prompt old-val perm-props)
   ""
   (let
      ((fmt perm-props))
      (case
	 (car fmt)
	 ;; Treat as a boolean
	 ((boolean)
	    (firrtl-dbg-read-new-boolean-val prompt old-val))
	 ((enum)
	    (firrtl-dbg-read-new-enum-val prompt fmt))
	 
	 (otherwise
	    (firrtl-dbg-read-new-decimal-val prompt old-val)))))

(defun firrtl-dbg-poke-value (sym new-val
				&optional extra-proc extra-data)
   "Poke NEW-VAL into the component named by SYM
Record the new value.  If EXTRA-PROC is non-nil, call it with extra-data."
   
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (firrtl-dbg-complain-bad-buffer))

   (let* 
      (  
	 (component (symbol-value sym))
	 (component-name (firrtl-dbg-input-full-name component))
	 (current (firrtl-dbg-input-current component))
	 (msg (concat "poke " component-name " "
		 (number-to-string new-val) "\n")))
      
      ;; IMPROVE ME:  Pre-filter inputs so we don't get errors here.
      (tq-enqueue firrtl-dbg-tq
	 msg
	 firrtl-dbg-tq-regexp
	 (list current new-val extra-proc extra-data)
	 #'(lambda (data str)
	      (let* 
		 ((had-problem
		     (firrtl-dbg-parse-response-maybe-complain str))
		    (current (first data))
		    (new-val (second data))
		    (extra-proc (third data))
		    (extra-data (fourth data)))

		 (unless had-problem
		    ;; Set the component's value to that.
		    (setf (firrtl-dbg-value-v current) new-val)
		    (setf (firrtl-dbg-value-state current) 'set-by-user-now)

		    (when extra-proc
		       (apply extra-proc extra-data))))))))

(defun firrtl-dbg-do-integer-edit&poke (widget widget-again &optional event)
   ""
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (firrtl-dbg-complain-bad-buffer))
   ;; Can replace most of it with this:
   '(firrtl-dbg-poke-value
       sym new-val
       #'(lambda (widget widgets-buffer)
	    (with-current-buffer widgets-buffer
	       (widget-value-set widget (widget-value widget))))
       (list widget (current-buffer)))

   (let* 
      (  (sym (widget-get widget :value))
	 (component (symbol-value sym))
	 (component-name (firrtl-dbg-input-full-name component))
	 (current (firrtl-dbg-input-current component))
	 (perm-props
	    (firrtl-dbg-get-perm-props (symbol-name sym)))
	 (new-val (firrtl-dbg-read-new-val
		     (format "New value for %s: " component-name)
		     current
		     perm-props))
	 (msg (concat "poke " component-name " "
		 (number-to-string new-val) "\n")))

      (when firrtl-dbg-writing-script-p
	 (setq firrtl-dbg-current-script-rv
	    (cons `(poke ,component-name ,new-val)
	       firrtl-dbg-current-script-rv)))

      ;; IMPROVE ME:  Pre-filter inputs so we don't get errors here.
      (tq-enqueue firrtl-dbg-tq
	 msg
	 firrtl-dbg-tq-regexp
	 (list current widget new-val (current-buffer))
	 #'(lambda (data str)
	      (let* 
		 ((had-problem
		     (firrtl-dbg-parse-response-maybe-complain str))
		    (current (first data))
		    (widget (second data))
		    (new-val (third data))
		    (widgets-buffer (fourth data)))

		 (unless had-problem
		    ;; Set the component's value to that.
		    (setf (firrtl-dbg-value-v current) new-val)

		    (setf (firrtl-dbg-value-state current) 'set-by-user-now)
		    (with-current-buffer widgets-buffer
		       (widget-value-set widget (widget-value widget)))))))))

(defun firrtl-dbg-run-script (script)
   "Run SCRIPT.
Script should be a list whose entries are in on of the forms:
 (poke name val)
 (step)
Where NAME is a string naming a component.
"
   (dolist (line script)
      (case (first line)
	 (poke
	    (firrtl-dbg-poke-value
	       (intern (second line) firrtl-dbg-obarray)
	       (third line)))
	 
	 (step
	    (firrtl-dbg-step-circuit))))

   ;; All done, now redisplay everything.  Mostly borrowed from
   ;; firrtl-dbg-initial-load, but redraws widgets instead of doing
   ;; initial draw.  REFACTOR ME.
   (tq-enqueue firrtl-dbg-tq "show\n" firrtl-dbg-tq-regexp
      (list (current-buffer))
      #'(lambda (data str)
	   (with-current-buffer (first data)
	      (unless (eq firrtl-dbg-current-buffer-type 'main)
		 (firrtl-dbg-complain-bad-buffer))
	      (firrtl-dbg-build-data str)
	      (firrtl-dbg-redraw-widgets)))))


;; Examples:
;; (firrtl-dbg-run-script
;;    '((poke "io_value1" 4)
;;        (poke "io_value2" 12)))

;; (firrtl-dbg-run-script
;;    '((step)))

(defun firrtl-dbg-start-recording-script ()
   ""

   (interactive)
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (firrtl-dbg-complain-bad-buffer))
   (setq firrtl-dbg-current-script-rv '())
   (setq firrtl-dbg-writing-script-p t))

(defun firrtl-dbg-stop-recording-script ()
   ""
   
   (interactive)
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (firrtl-dbg-complain-bad-buffer))
   (setq firrtl-dbg-writing-script-p nil))

(defun firrtl-dbg-get-script ()
   ""
   
   (interactive)
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (firrtl-dbg-complain-bad-buffer))
   (with-output-to-temp-buffer "*FIRRTL script*"
      (princ ";;User-generated script\n")
      (princ ";;Call as '(firrtl-dbg-run-script SCRIPT)'\n")
      (princ "\n")
      (prin1
	 (reverse firrtl-dbg-current-script-rv))))


(defun firrtl-dbg-parse-response-maybe-complain (str)
   "Return non-nil if str caused an error message"
   
   (let*
      (
	 (legit-rx (concat " *" firrtl-dbg-tq-prompt-string))
	 (start-legit (string-match legit-rx str)))
      ;; Show any error that we get back
      (when (or (null start-legit) (> start-legit 0))
	 (message "%s" (substring str 0 start-legit))
	 t)))


;;;;;;;;;;;;;;;;;;;;

;; Example:
;; (firrtl-dbg-call-until-done-w/timeout 4
;;    #'(lambda (msg)
;; 	(message msg) nil)
   
;;    (list "Aloha!")
;;    #'(lambda (msg)
;; 	(message "For the last time: %s" msg))
;;    (list "Aloha!"))

(defstruct (firrtl-dbg-timer-data (:type list))
   ""
   seconds-to-wait
   timer)


(defun firrtl-dbg-call-until-done-w/timeout
   (num-seconds proc args &optional timed-out-proc timed-out-args)
   "
PROC should return non-nil if it has finished its work"
   (let
      ((data (make-firrtl-dbg-timer-data :seconds-to-wait num-seconds)))
      (setf (firrtl-dbg-timer-data-timer data)
	 (run-at-time t 1
	    #'(lambda (data proc args timed-out-proc timed-out-args)
		 ;; Manage timeout
		 (if (<= (firrtl-dbg-timer-data-seconds-to-wait data) 0)
		    (progn
		       (cancel-timer (firrtl-dbg-timer-data-timer data))
		       (when timed-out-proc
			  (apply timed-out-proc timed-out-args)))
		    (let
		       ((done (apply proc args)))
		       (if done
			  (cancel-timer (firrtl-dbg-timer-data-timer data))
			  (decf (firrtl-dbg-timer-data-seconds-to-wait data))))))
	    data
	    proc args
	    timed-out-proc timed-out-args))))

(defun firrtl-dbg-process-is-ready-p (process)
   "True if the firrtl-dbg process is ready, meaning that it has arrived at its initial prompt.  This may take a while."
   (with-current-buffer (process-buffer process)
      (goto-char (point-min))
      (search-forward firrtl-dbg-tq-prompt-string nil t)))

(defun firrtl-dbg-initial-load ()
   ""
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (firrtl-dbg-complain-bad-buffer))

   (tq-enqueue firrtl-dbg-tq "show\n" firrtl-dbg-tq-regexp
      (list (current-buffer))
      #'(lambda (data str)
	   (with-current-buffer (first data)
	      (unless (eq firrtl-dbg-current-buffer-type 'main)
		 (firrtl-dbg-complain-bad-buffer))
	      (firrtl-dbg-build-data str)
	      (firrtl-dbg-create-widgets)))))

(define-derived-mode firrtl-dbg-mode
   special-mode "Firrtl-Dbg"
   "Major mode for FIRRTL debugger interface"
   :group 'firrtl-dbg
   (progn
      (set-keymap-parent firrtl-dbg-mode-map widget-keymap)
      (define-key firrtl-dbg-mode-map  "\M-\r"
	 #'firrtl-dbg-do-alt-interaction)))


(defun firrtl-dbg-complain-bad-buffer (&optional msg)
   ""
   ;; PUT ME BACK when ready
   ;;(error (or msg "This operation only makes sense in main buffer"))
   (message
      (or msg "This operation only makes sense in a main circuit buffer")))

(defmacro firrtl-dbg-local-defvar (name value docstring)
   "Define VAR as a buffer-local variable with default value VAL.
This is different than defvar-local in that it doesn't define the variable in other buffers."
   
   `(progn
       (set (make-local-variable ',name) ,value)
       (put ',name 'variable-documentation ,docstring)))

(defun firrtl-dbg-startup (working-directory repl-launch-command)
   ""
   (interactive
      (list
	 (let
	    ((file-name-history firrtl-dbg-directory-history))
	    (read-directory-name "Working directory: "))
	 (read-string "FIRRTL REPL in Scala: " nil
	    'firrtl-dbg-repl-name-history)))
   
   
   (let*
      (
	 (buf-name (concat "*FIRRTL " repl-launch-command "*" ) )
	 (main-buf
	    (generate-new-buffer buf-name)))
      (with-current-buffer main-buf
	 (firrtl-dbg-mode)
	 (setq default-directory working-directory)
	 ;; Set up most of the local variables.  Some are set further
	 ;; down as their objects are created.
	 (set (make-local-variable 'firrtl-dbg-current-buffer-type)
	    'main)

	 (firrtl-dbg-local-defvar firrtl-dbg-obarray
	    (make-vector firrtl-dbg-obarray-default-size nil)
	    "Obarray that holds the current data of FIRRTL components")

	 (firrtl-dbg-local-defvar firrtl-dbg-obarray-perm-props
	    (make-vector firrtl-dbg-obarray-default-size nil)
	    "Obarray that holds data about FIRRTL components that persists between sessions")

	 (firrtl-dbg-local-defvar firrtl-dbg-perm-props-alist
	    '()
	    "Alist that holds data that persists between sessions about FIRRTL components")

	 (firrtl-dbg-local-defvar firrtl-dbg-subname-tree
	    '()
	    "The component-tree of the circuit.

Format: Each node is either:
  (subname-string t list-of-nodes)
  (subname-string nil . sym)"
	    )

	 (firrtl-dbg-local-defvar firrtl-dbg-have-built-subname-tree
	    nil
	    "Whether the subname tree has been built yet")
	 
	 (firrtl-dbg-local-defvar firrtl-dbg-current-step
	    nil
	    "The current step of the circuit")

	 (firrtl-dbg-local-defvar firrtl-dbg-current-freshness
	    "UNKNOWN"
	    "The current freshness of the circuit, as a string")
	 
	 (firrtl-dbg-local-defvar firrtl-dbg-writing-script-p
	    nil
	    "Whether we are currently writing a script")
	 
	 (firrtl-dbg-local-defvar firrtl-dbg-current-script-rv
	    '()
	    "The script that we are currently writing, in reverse order")
	 
	 (firrtl-dbg-local-defvar firrtl-dbg-process-buffer
	    (generate-new-buffer firrtl-dbg-process-buffer-name)
	    "The buffer of the FIRRTL REPL process")

	 (with-current-buffer firrtl-dbg-process-buffer
	    (setq default-directory working-directory)
	    (set
	       (make-local-variable 'firrtl-dbg-main-buffer)
	       main-buf))

	 (firrtl-dbg-local-defvar firrtl-dbg-process
	    (let ((default-directory working-directory))
	       (start-process
		  firrtl-dbg-process-name
		  firrtl-dbg-process-buffer
		  firrtl-dbg-executable
		  ;; Quoting this string with shell-quote-argument
		  ;; actually messes us up.
		  repl-launch-command))
	    "The FIRRTL REPL process")

	 (firrtl-dbg-local-defvar firrtl-dbg-tq
	    nil
	    "The firrtl-dbg transaction queue")

	 (hack-dir-local-variables-non-file-buffer)
	 (firrtl-dbg-copy-alist-to-perms)
	 
	 (firrtl-dbg-call-until-done-w/timeout
	    40
	    #'(lambda (process main-buf)
		 (when
		    (firrtl-dbg-process-is-ready-p process)
		    (message "Debugger process is ready")
		    (let* 
		       ((tq (tq-create process)))
		       (with-current-buffer main-buf
			  (setq firrtl-dbg-tq tq)
			  (firrtl-dbg-initial-load)))
		    (pop-to-buffer main-buf)
		    ;; Indicate that we have succeeded
		    t))
	    (list firrtl-dbg-process main-buf)
	    #'(lambda ()
		 (message "Debugger process timed out"))
	    '()))))




;;;_. Footers
;;;_ , Provides

(provide 'firrtl-debugger)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: emacs-lisp
;;;_  + End:

;;;_ , End
;;; firrtl-debugger.el ends here
