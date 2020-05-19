;;;_ treadle-dbg.el --- Treadle debugger interface

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

;; IN FLUX.  This will now be aimed at the treadle debugger.

;; This is an emacs interface for the Treadle debugger REPL for
;; Chisel.  To use it, you will need Chisel3 and sbt.  If you don't
;; know what those are, you probably don't need this package.

;; The entry point is 'firrtl-dbg'.  You need to have already set up
;; Treadle in Scala.

;; Instructions for setting up and using Treadle itself can be found
;; at https://www.chisel-lang.org/treadle/
;; 
;; This is just a more convenient interface to that.

;; You'll have to point it towards the right directory (the one that
;; build.sbt lives in) You probably want to set
;; firrtl-dbg-directory-history and firrtl-dbg-repl-name-history to
;; that your current project pops up at the top of the history lists
;; when calling firrtl-dbg.

;; It doesn't support vpn scripts, but does support a native elisp
;; script.  Create it with firrtl-dbg-start-recording-script, do stuff
;; in the main buffer, then firrtl-dbg-stop-recording-script when
;; done.  It will pop up a buffer with the script in it.  It's on you
;; to copy that code somewhere.  Run it with firrtl-dbg-run-script.
;; You can also run handcrafted scripts, or scripts generated from
;; Scala but then you're going to have to write the print statements
;; and the conversion yourself.

;; At this point, there are a lot of features still to be added, and I
;; can't promise that I will ever get around to adding them.

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

;; Now all components are this type
(defstruct (treadle-dbg-component (:type list))
   ""
   full-name
   source
   ;; These can be nil if it doesn't do that.
   current
   prev
   in
   in/prev
   signed-p
   width
   io-type ;; '(input output clock reset nil)
   forced-p ;; Whether it is currently forced.
   )

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations and constants

(defgroup firrtl-dbg nil "Customizations for Firrtl-dbg"
   :group 'applications)

(defface firrtl-dbg-face-value-poison '((t :background "gray"))
   "The face for poisoned values"
   :group 'treadle-dbg)

(defface firrtl-dbg-face-value-set-by-user-earlier
   '((t (:foreground "forest green")))
   "The face for values set earlier"
   :group 'treadle-dbg)

(defface firrtl-dbg-face-value-set-by-user-now
   '((t (:background "LightCyan1")))
   "The face for values set since the last step"
   :group 'treadle-dbg)

(defface firrtl-dbg-face-value-default '()
   "The face for normal values"
   :group 'treadle-dbg)

(defface treadle-dbg-face-value-input-unset '((t :background "gray"))
   "The face for poisoned values"
   :group 'treadle-dbg)

(defface treadle-dbg-face-forced-noninput-value
   '((t (:foreground "forest green")))
   "The face for values set earlier"
   :group 'treadle-dbg)

(defface treadle-dbg-face-value-set-by-user-now
   '((t (:background "LightCyan1")))
   "The face for values set since the last step"
   :group 'treadle-dbg)

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
   :group 'treadle-dbg)

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
   :group 'treadle-dbg)

(defcustom firrtl-dbg-repl-name-history
   '("test:runMain gcd.GCDRepl")
   "History list of FIRRTL REPL commands.  Put the full command that you would give sbt to run a FIRRTL debugger on this list.  Then 'firrtl-dbg-startup' will see it as a history item"
   :type '(repeat string)
   :group 'treadle-dbg)

(defconst treadle-dbg-repl-launch-command
   "runMain treadle.TreadleRepl"
   "" )

;;;;;;;;;;;;;;;;;;;;
;;Configuration

(defcustom firrtl-dbg-executable
   "sbt"
   "Name of the actual executable that helps launch the debugger REPL"
   :type 'string
   :group 'treadle-dbg)


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
   :group 'treadle-dbg)

(defconst firrtl-dbg-type-regexp
   "type \\([^ ]+\\) \\([0-9]+\\).\\([A-Z]+\\)<\\([0-9]+\\)>"
   "Regexp matching the string returned by the 'type' command" )

(defconst firrtl-dbg-tq-regexp
   (concat ".*" firrtl-dbg-tq-prompt-string " *")
   "Regexp matching any response from the REPL" )

(defconst firrtl-dbg-prompt-line-regexp
   (concat "^" firrtl-dbg-tq-prompt-string " *")
   "Regexp matching a bare prompt line from the REPL" )

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Widgets
(defvar-local firrtl-dbg-widget-of-step-num
   nil
   "Widget displaying the current step value")

(defvar-local firrtl-dbg-widget-of-freshness
   nil
   "Widget displaying the current freshness")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data items
;; RE-LOCALIZE ME:  All must become defvar-local again
(defvar treadle-dbg-spurious-lines
   '()
   "Spurious lines from stepping")

(defvar treadle-dbg-obarray
   (make-vector firrtl-dbg-obarray-default-size nil)
   "Obarray that holds the current treadle data of FIRRTL components")
'
(defvar firrtl-dbg-obarray
   (make-vector firrtl-dbg-obarray-default-size nil)
   "Obarray that holds the current data of FIRRTL components")

(defvar treadle-dbg-obarray-perm-props
   (make-vector firrtl-dbg-obarray-default-size nil)
   "Obarray that holds data about FIRRTL components that persists between sessions")

(defvar treadle-dbg-perm-props-alist
   '()
   "Alist that holds data that persists between sessions about FIRRTL components")

(defvar treadle-dbg-subname-tree
   '()
   "The component-tree of the circuit.

Format: Each node is either:
  (subname-string t list-of-nodes)
  (subname-string nil . sym)"
   )

(defvar treadle-dbg-have-built-subname-tree
   nil
   "Whether the subname tree has been built yet")

(defvar treadle-dbg-current-step
   nil
   "The current step of the circuit")

(defvar treadle-dbg-current-freshness
   "UNKNOWN"
   "The current freshness of the circuit, as a string")

(defvar treadle-dbg-writing-script-p
   nil
   "Whether we are currently writing a script")

(defvar treadle-dbg-current-script-rv
   '()
   "The script that we are currently writing, in reverse order")

(defvar treadle-dbg-process-buffer
   nil
   "The buffer of the FIRRTL REPL process")

(defvar treadle-dbg-main-buffer
   nil
   "The main interaction buffer")

(defvar treadle-dbg-tq
   nil
   "The firrtl-dbg transaction queue")

(defvar treadle-dbg-process
   nil
   "The FIRRTL REPL process")

;;;;;;;;;;;;;;;;;;;;

(defun treadle-dbg-add-to-subname-tree (tree subname-list data)
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
		     (push current-tag+tree subtree-info-list)
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
	 (push current-tag+tree subtree-info-list)
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


;; (treadle-dbg-add-to-subname-tree '() '("a" "b")
;;    'my-data)


;; (treadle-dbg-add-to-subname-tree '(list ("a" list ("b" my-data))) '("a" "b")
;;     'new-data)


;; (treadle-dbg-add-to-subname-tree '(list ("a" list ("b" my-data))) '("a" "c")
;;     'new-data)


;; (treadle-dbg-add-to-subname-tree '(list ("a" list ("b" my-data))) '("d" "b")
;;     'new-data)

(defun treadle-dbg-split-component-name (str)
   ""
   (split-string str "[._]+"))

;; (treadle-dbg-split-component-name "io_a.b")

'
(progn
   (setq str1 "cellVec2_2.timeVec_3.clock/prev 0")
   (setq str-an-output "io_operationWhatMyLastDebug 1")
   (setq str-an-input "io_operation 0"))



(defstruct treadle-dbg-state-entry
   ""
   full-name
   split-name
   qualifiers
   value
   )

;; Actually not needed, treadle-dbg-str->state-entry does all
;; (defun treadle-dbg-show-output-result->state-entry (str)
;;    ""
;;    (treadle-dbg-str->state-entry str))
;; (defun treadle-dbg-show-input-result->state-entry (str)
;;    ""
;;    (treadle-dbg-str->state-entry str))
;; '
;; (treadle-dbg-show-output-result->state-entry str-an-output)
;; '
;; (treadle-dbg-show-input-result->state-entry str-an-input)


(defun treadle-dbg-str->state-entry (str)
   ""
   (string-match "^\\([^ ]+\\) +\\([0-9]+\\)" str)
   
   (let*
      (
	 (name-plus (match-string 1 str))
	 (value (string-to-number (match-string 2 str)))
	 (spl (split-string name-plus "/"))
	 (full-name (car spl))
	 (qualifiers nil))

      (dolist (q (cdr spl))
	 (cond
	    ((string-equal q "in")
	       (setq qualifiers
		  (case qualifiers
		     ((nil) 'in)
		     ((in)  'in)
		     ((prev in/prev)  'in/prev))))
	    ((string-equal q "prev")
	       (setq qualifiers
		  (case qualifiers
		     ((nil) 'prev)
		     ((in in/prev)  'in/prev)
		     ((prev)  'prev))))))
      
      (make-treadle-dbg-state-entry
	 :full-name full-name
	 :split-name (treadle-dbg-split-component-name full-name)
	 :qualifiers qualifiers
	 :value value)))
'
(treadle-dbg-str->state-entry str1)


(defun treadle-dbg-mutate-component-value (component e)
   ""

   (let*
      ((value (treadle-dbg-state-entry-value e)))
      (case (treadle-dbg-state-entry-qualifiers e)
	 ((nil)
	    (setf (treadle-dbg-component-current component) value))
	 ((in)
	    (setf (treadle-dbg-component-in component) value))
	 ((prev)
	    (setf (treadle-dbg-component-prev component) value))
	 ((in/prev)
	    (setf (treadle-dbg-component-in/prev component) value)))))


(defun treadle-dbg-mutate-subname-tree (full-name data)
   ""
   ;; Temporarily out
   '
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer
	 "Objects are only available in the main buffer"))

   (setq
      treadle-dbg-subname-tree
      (treadle-dbg-add-to-subname-tree treadle-dbg-subname-tree
	 (treadle-dbg-split-component-name full-name)
	 data)))
'
(defun firrtl-dbg-add-object (full-name proc-mutate proc-create)
   ""
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer
	 "Objects are only available in the main buffer"))

   (let* 
      (
	 (soft-sym (intern-soft full-name firrtl-dbg-obarray))
	 (sym (or soft-sym (intern full-name firrtl-dbg-obarray))))
      (if soft-sym
	 (funcall proc-mutate (symbol-value sym))
	 ;; Since it doesn't exist, create it
	 (set sym (funcall proc-create)))
      
      (when (not treadle-dbg-have-built-subname-tree)
	 (treadle-dbg-mutate-subname-tree full-name sym))))

(defun treadle-dbg-add-object (full-name proc-mutate)
   ""
    ;; Temporarily out
   '
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer
	 "Objects are only available in the main buffer"))

   (let* 
      (
	 (soft-sym (intern-soft full-name treadle-dbg-obarray))
	 (sym (or soft-sym (intern full-name treadle-dbg-obarray))))
      (if soft-sym
	 (funcall proc-mutate (symbol-value sym))
	 (let* 
	    ((component
		(make-treadle-dbg-component :full-name full-name)))
	    (funcall proc-mutate component)
	    (set sym component)))
      
      (when (not treadle-dbg-have-built-subname-tree)
	 (treadle-dbg-mutate-subname-tree full-name sym))))

'
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
'
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

'
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
'
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
'
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
'
(defun firrtl-dbg-read-overview-line (str)
   ""
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))

   (let* 
      (  (m (string-match "CircuitState \\([0-9]+\\) (\\([A-Z]+\\))" str))
	 (step (string-to-number (match-string 1 str)))
	 (freshness-str (match-string 2 str)))
   
      (setq treadle-dbg-current-freshness freshness-str)
      (setq treadle-dbg-current-step step)))


'
(defun firrtl-dbg-split-input-line (input-str prefix-rx)
   ""
   (let* 
      (
	 (m (string-match prefix-rx input-str))
	 (end (match-end 0))
	 (input-str (substring input-str end)))
      (split-string input-str ",")))
'
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

(defun treadle-dbg-set-data-aux (state-string mutator)
   "
MUTATOR takes two arguments.  First is a treadle-dbg-component, second is a treadle-dbg-state-entry"
   (let*
      ((spl (split-string state-string "\n")))
      (dolist (line spl)
	 (unless (string-empty-p line)
	    (let* 
	       ((e (treadle-dbg-str->state-entry line)))
	       (treadle-dbg-add-object
		  (treadle-dbg-state-entry-full-name e)
		  #'(lambda (component)
		       (funcall mutator component e))))))))

;; Processes the return string from "show state"
(defun treadle-dbg-record-state (state-string)
   "Set the data of components from the result of Treadle 'show state'"
   (treadle-dbg-set-data-aux
      state-string
      #'treadle-dbg-mutate-component-value))


(defun treadle-dbg-record-inputs (str)
   "Set the data of components from the result of Treadle 'show inputs'"
   (treadle-dbg-set-data-aux
      str
      #'(lambda (component entry)
	   (let* 
	      ((name (treadle-dbg-state-entry-full-name entry))
		 (type
		    (cond
		       ((string-equal name "reset") 'reset)
		       ((string-equal name "clock") 'clock)
		       (t 'input))))
	      (setf (treadle-dbg-component-io-type component) type)))))


(defun treadle-dbg-record-outputs (str)
   "Set the data of components from the result of Treadle 'show outputs'"
   (treadle-dbg-set-data-aux
      str
      #'(lambda (component entry)
	   (setf (treadle-dbg-component-io-type component) 'output))))
;; Call symbol on ^foo$.  Use the original name, because symbol truncates the longer ones.  FIX ME: Not everything has source lines, so change the regexp to account for that.  Not clear how.
'
(progn
   (setq state-string1
      "cellVec2_2.timeVec_3.clock 0
cellVec2_2.timeVec_3.clock/prev 0
cellVec2_2.timeVec_3.io_writeEnableReachesHere 0
cellVec2_2.timeVec_3.reset 0
clock 0
clock/prev 0
io_operation 0
io_operationWhatMyLastDebug 0
io_pulseOnPerCell_0 0
io_pulseOnPerCell_1 0
io_pulseOnPerCell_2 0
lastOperation 0
lastOperation/in 0")
   (setq show-inputs-string
      "clock 0
io_operation 0
reset 0
"
      )
   (setq show-outputs-string
      "io_operationWhatMyLastDebug 1
io_pulseOnPerCell_0 0
io_pulseOnPerCell_1 0
io_pulseOnPerCell_2 0
")
   (setq symbol-string-lastOperation
      "Name                                     Bin Type  Width  Slots  Index Depend Info
lastOperation                            Int UInt      3      1    252I      3  @[dummy.scala 262:30] 0
lastOperation/in                         Int UInt      3      1    287I      6  @[dummy.scala 262:30] 0
"
      )

   ;; Slight cheat: Un-truncated the names for this.
   ;; Redo this for the new format info.
   (mapcar
      ;;#'treadle-dbg-test-second-symbol-line
      #'treadle-dbg-record-symbol-info
      '("Name                                     Bin Type  Width  Slots  Index Depend Info
cellVec2_2.timeVec_3.clock               Int UInt      1      1    238I     62  0
""Name                                     Bin Type  Width  Slots  Index Depend Info
cellVec2_2.timeVec_3.io_writeEnableReachesHere Int UInt      1      1     45I     61  0
""Name                                     Bin Type  Width  Slots  Index Depend Info
cellVec2_2.timeVec_3.reset               Int UInt      1      1    356I     67  0
""Name                                     Bin Type  Width  Slots  Index Depend Info
clock                                    Int UInt      1      1    212I      2  0
""Name                                     Bin Type  Width  Slots  Index Depend Info
io_operation                             Int UInt      3      1     84I      1  0
""Name                                     Bin Type  Width  Slots  Index Depend Info
io_operationWhatMyLastDebug              Int UInt      3      1    230I      4  0
""Name                                     Bin Type  Width  Slots  Index Depend Info
io_pulseOnPerCell_0                      Int UInt      2      1    161I     -1  0
""Name                                     Bin Type  Width  Slots  Index Depend Info
io_pulseOnPerCell_1                      Int UInt      2      1      9I     -1  0
""Name                                     Bin Type  Width  Slots  Index Depend Info
io_pulseOnPerCell_2                      Int UInt      2      1    229I     -1  0

""Name                                     Bin Type  Width  Slots  Index Depend Info
lastOperation                            Int UInt      3      1    252I      3  @[dummy.scala 262:30] 0
"
"Name                                     Bin Type  Width  Slots  Index Depend Info
reset                                    Int UInt      1      1    212I      2  0
"
	  )
      )

   (treadle-dbg-record-state state-string1)
   (treadle-dbg-record-outputs show-outputs-string)
   (treadle-dbg-record-inputs show-inputs-string)
   (treadle-dbg-record-symbol-info symbol-string-lastOperation)
   (treadle-dbg-test-second-symbol-line symbol-string-lastOperation)

   '
   (string-match
	       "\\([^ ]+\\) +\\([A-Za-z]+\\) +\\([A-Za-z]+\\) +\\([0-9]+\\) +\\([0-9]+\\) +\\([^ ]+\\) +\\([0-9]+\\) +@\\[\\([^[]+\\)\\] +\\([0-9]+\\)"

      (second (split-string symbol-string-lastOperation "\n")))
   
   )

(defstruct (symbol-record-strings (:type list))
   ""
   trunc-name type-str width-str source-str value-str
   ;; Not using: bin slots index depend
   )
(defun treadle-dbg-test-second-symbol-line (symbol-string)
   ""
   
   (let*
      ((spl (split-string symbol-string "\n")))
      (treadle-dbg-symbol-string->struct (second spl))))

(defun treadle-dbg-symbol-string->struct (line)
   ""
   (let* 
      ((pos 0)
	 (strings (make-symbol-record-strings)))
      ;; Skip any initial whitespace
      (string-match " *" line pos)
      (setq pos (match-end 0))
      ;; Match truncated name
      (string-match "[^ ]+" line pos)
      (setf (symbol-record-strings-trunc-name strings)
	 (substring line pos (match-end 0)))
      (setq pos (match-end 0))
      ;; Skip whitespace
      (string-match " +" line pos)
      (setq pos (match-end 0))
      ;; Match bin
      (string-match "[a-zA-Z]+" line pos)
      ;;(setq bin-str (substring line pos (match-end 0)))
      (setq pos (match-end 0))
      ;; Skip whitespace
      (string-match " +" line pos)
      (setq pos (match-end 0))
      ;; Match type-str
      (string-match "[a-zA-Z]+" line pos)
      (setf (symbol-record-strings-type-str strings)
	 (substring line pos (match-end 0)))
      (setq pos (match-end 0))
      ;; Skip whitespace
      (string-match " +" line pos)
      (setq pos (match-end 0))
      ;; Match width
      (string-match "[0-9]+" line pos)
      (setf (symbol-record-strings-width-str strings)
	 (substring line pos (match-end 0)))
      (setq pos (match-end 0))
      ;; Skip whitespace
      (string-match " +" line pos)
      (setq pos (match-end 0))
      ;; Match slots
      (string-match "[0-9]+" line pos)
      ;;(setq slots-str (substring line pos (match-end 0)))
      (setq pos (match-end 0))
      ;; Skip whitespace
      (string-match " +" line pos)
      (setq pos (match-end 0))
      ;; Match index
      (string-match "[^ ]+" line pos)
      ;;(setq index-str (substring line pos (match-end 0)))
      (setq pos (match-end 0))
      ;; Skip whitespace
      (string-match " +" line pos)
      (setq pos (match-end 0))
      ;; Match depend
      (string-match "[^ ]+" line pos)
      ;;(setq depend-str (substring line pos (match-end 0)))
      (setq pos (match-end 0))
      ;; Skip whitespace
      (string-match " +" line pos)
      (setq pos (match-end 0))
      ;; Match source if available.
      ;; Match beginning of source
      (string-match "^@\\[" line pos)
      (when (match-beginning 0)
	 (let* 
	    ((begin (match-end 0)))
	    ;; Find the end bracket
	    (string-match "\\]" line pos)
	    ;; Everything in between is our source.
	    (setf (symbol-record-strings-source-str strings)
	       (substring line begin (match-end 0)))
	    (setq pos (match-end 0))
	    ;; Skip whitespace.  Either way 'pos' will stand
	    ;; after whitespace.
	    (string-match " +" line pos)
	    (setq pos (match-end 0))))
      ;; Match value
      (string-match "[0-9]+" line pos)
      (setf (symbol-record-strings-value-str strings)
	 (substring line pos (match-end 0)))
      (setq pos (match-end 0))

      strings))

(defun treadle-dbg-record-symbol-info (symbol-string)
   ""
   (let*
      ((spl (split-string symbol-string "\n")))
      (unless (string-match-p
		 "Name +Bin +Type +Width +Slots +Index +Depend +Info"
		 (car spl))
	 (error "This symbol report is not in the expected format: %S"
	    (car spl)))
      
      (dolist (line (cdr spl))
	 (unless (string-empty-p line)
	    (let* 
	       ((info (treadle-dbg-symbol-string->struct line))
		  ;; TEMPORARY.  We'll take the real namestring as an arg
		  (name (symbol-record-strings-trunc-name info)))
	       
	       (unless
		  ;; Don't use foo/in, foo/prev, etc
		  (string-match-p ".*/.*"
		     (symbol-record-strings-trunc-name info))
		  ;; This bug seems because we expected a source-line piece.
		  (treadle-dbg-add-object name
		     ;; Proc mutate
		     #'(lambda (component)
			  (let* 
			     (
				(source-str
				    (symbol-record-strings-source-str info))
				(type-str
				    (symbol-record-strings-type-str info))
				(value-str
				    (symbol-record-strings-value-str info))
				(signed-p
				   (cond
				      ((string-equal type-str "UInt") nil)
				      ((string-equal type-str "SInt") t)
				      (t (message "Unrecognized type-str"))))
				(width
				   (string-to-number
				      (symbol-record-strings-width-str info))))
			     (unless
				(eql (treadle-dbg-component-current component)
				   (string-to-number value-str))
				(message "Values do not match!"))
			     (setf
				(treadle-dbg-component-width component)
				width)
			     (setf (treadle-dbg-component-signed-p component)
				signed-p)
			     (setf (treadle-dbg-component-source component)
				source-str)))))
	       )
	    ;;
))))



;; ADAPT AND UPDATE ME  This was the only thing, but Treadle-dbg does more.
'
(defun firrtl-dbg-build-data (state-string)
   ""

   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer
	 "Building the data only makes sense in a circuit buffer"))

   (let
      ((spl (split-string state-string "\n")))
      (dolist (line spl)
	 (cond
	    ((string-match "^step" line)
	       ;; We do nothing with step data yet
	       )
	    ;; Blank line, nothing to do
	    ((string-match "^[ \t]*$" line))
	    
	    ((string-match "^CircuitState" line)
	       (firrtl-dbg-read-overview-line line))

	    ((string-match "^Inputs *: *" line)
	       (mapcar
		  #'(lambda (v)
		       (firrtl-dbg-act-on-component-str
			  v #'firrtl-dbg-add-input))
		  (split-string (substring line (match-end 0)) ",")))

	    ((string-match "^Outputs *: *" line)
	       (mapcar
		  #'(lambda (v)
		       (firrtl-dbg-act-on-component-str
			  v #'firrtl-dbg-add-output))
		  (split-string (substring line (match-end 0)) ",")))

	    ((string-match "^Registers *: *" line)
	       (mapcar
		  #'(lambda (v)
		       (firrtl-dbg-act-on-component-str
			  v #'firrtl-dbg-set-register-current))
		  (split-string (substring line (match-end 0)) ",")))

	    ((string-match "^FutureRegisters *: *" line)
	       (mapcar
		  #'(lambda (v)
		       (firrtl-dbg-act-on-component-str
			  v #'firrtl-dbg-set-register-next))
		  (split-string (substring line (match-end 0)) ",")))

	    ((string-match "^Ephemera *: *" line)
	       (mapcar
		  #'(lambda (v)
		       (firrtl-dbg-act-on-component-str
			  v #'firrtl-dbg-add-ephemeral))
		  (split-string (substring line (match-end 0)) ",")))

	    ((string-match "^Memories *:? *" line)
	       ;; We do nothing with memories yet
	       )
	    ((string-match firrtl-dbg-prompt-line-regexp line)
	       ;; That's the prompt line, it's not part of the actual
	       ;; response.  Ignore it.
	       )
	    (t
	       ;; IMPROVE ME: Collect these lines, they are probably
	       ;; debug printing.
	       (message "Spurious line: %s" line))))

      (when (not treadle-dbg-have-built-subname-tree)
	 ;; IMPROVE ME: Sort the newly built subname tree
	 )

      (setq treadle-dbg-have-built-subname-tree t)))

;; ADAPT ME
(defun firrtl-dbg-clear ()
   "Clear all the values; ready to start again"
   (interactive)

   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer
	 "Clearing the data only makes sense in a circuit buffer"))

   (setq treadle-dbg-have-built-subname-tree nil)
   (setq treadle-dbg-subname-tree '())
   (setq firrtl-dbg-obarray
      (make-vector firrtl-dbg-obarray-default-size nil))
   (setq treadle-dbg-spurious-lines '()))

;; ADAPT ME
(defun firrtl-dbg-shutdown ()
   ""
   
   (interactive)

   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer
	 "Shutting down only makes sense in a circuit buffer"))

   (firrtl-dbg-clear)
   
   (when treadle-dbg-tq
      (tq-close treadle-dbg-tq))
   (when treadle-dbg-process-buffer
      (kill-buffer treadle-dbg-process-buffer))
   (setq treadle-dbg-process-buffer nil)
   (kill-buffer (current-buffer)))

(defun treadle-dbg-pad-to-column (column face)
   ""
   
   (let*
      ((space (propertize " " 'face face)))
      (widget-insert space) ;; Force at least one space
      (while (< (current-column) column)
	 (widget-insert space))))

(defun treadle-dbg-insert-w-face (str face)
   ""
   
   (let*
      ((str (if face (propertize str 'face face) str)))

      (widget-insert str)))
'
(defun firrtl-dbg-insert-fields (field-list)
   "FIELD-LIST is a list of ((text face end-col) ...)

END-COL is the column to start the next field at.  Face is
applied up until that column."
   
   (dolist (field field-list)
      (when field
	 (if (stringp field)
	    (widget-insert field)
	    (destructuring-bind (text face end-col) field
	       (treadle-dbg-insert-w-face text face)
	       (treadle-dbg-pad-to-column end-col face))))))

;; Want to fix the field treatment: pad to start-col with no face.
;; Insert with face.  Pad with face to end-col if any.  But that's
;; nasty if we sometimes have a chain in fields and sometimes don't.
;;
;; Perhaps better: Have separate "insert" and "go-to" instructions.

(defun treadle-dbg-insert-fields (field-list)
   "FIELD-LIST is a list whose elements are either
string
(to-col face col)
(string face)
"
   
   (dolist (field field-list)
      (cond
	 ((null field))
	 ((stringp field) (widget-insert field))
	 ((and (listp field) (eq (car field) 'to-col))
	    (destructuring-bind (dummy col &optional face) field
	       (treadle-dbg-pad-to-column col face)))
	 ((listp field)
	    (destructuring-bind (text face) field
	       (treadle-dbg-insert-w-face text face))))))

'
(defun firrtl-dbg-get-face-by-validity (validity)
   ""
   
   (case validity
      (poisoned 'firrtl-dbg-face-value-poison)
      (set-by-user-now 'firrtl-dbg-face-value-set-by-user-now)
      (set-by-user-earlier 'firrtl-dbg-face-value-set-by-user-earlier)
      (ok 'firrtl-dbg-face-value-default)
      (t nil)))

(defun treadle-dbg-enum-string (fmt index)
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

(defun treadle-dbg-value-text (value perm-props)
   ""
   (let* 
      (
	 (fmt perm-props)
	 (text
	    (case (first fmt)
	       ((boolean)
		  ;; ENCAP ME
		  (case value
		     (0 "false")
		     (1 "true")
		     (otherwise "[invalid]")))
	       ((enum)
		  (treadle-dbg-enum-string fmt value))
	       
	       (otherwise
		  (number-to-string
		     value)))))
      text))
'
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
'
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

(defun treadle-dbg-insert-component-aux (component perm-props)
   ""
   (let* 
      ((width-string (number-to-string (treadle-dbg-component-width component)))
	 (sign-string
	    (if (treadle-dbg-component-signed-p component) "S" "U"))
	 (face-of-current
	    (if
	       (eq (treadle-dbg-component-io-type component) 'input)
	       (if (treadle-dbg-component-forced-p component)
		  'treadle-dbg-face-value-set-by-user-now
		  'treadle-dbg-face-value-input-unset)
	       (if (treadle-dbg-component-forced-p component)
		  'treadle-dbg-face-forced-noninput-value
		  'firrtl-dbg-face-value-default)))
	 (field-list-rv '()))
      (push
	 (list
	    (treadle-dbg-component-full-name component)
	    nil)
	 field-list-rv)
      (push
	 (list 'to-col firrtl-dbg-value-column)
	 field-list-rv)
      (when (treadle-dbg-component-current component)
	 (push
	    (list
	       (treadle-dbg-value-text
		  (treadle-dbg-component-current component)
		  perm-props)
	       face-of-current)
	    field-list-rv))
      (when (treadle-dbg-component-prev component)
	 (push " <- " field-list-rv)
	 (push
	    (treadle-dbg-value-text
	       (treadle-dbg-component-prev component)
	       perm-props)
	    field-list-rv))
      (when (treadle-dbg-component-in component)
	 (push " << " field-list-rv)
	 (push
	    (treadle-dbg-value-text
	       (treadle-dbg-component-in component)
	       perm-props)
	    field-list-rv)
	 (when (treadle-dbg-component-in/prev component)
	    (push " <- " field-list-rv)
	    (push
	       (treadle-dbg-value-text
		  (treadle-dbg-component-in/prev component)
		  perm-props)
	       field-list-rv)))

      (push
	 (list 'to-col (1- firrtl-dbg-value-end-column) face-of-current)
	 field-list-rv)
      (push
	 (list 'to-col firrtl-dbg-value-end-column nil)
	 field-list-rv)
      
      (push width-string field-list-rv)
      (push sign-string field-list-rv)

      (treadle-dbg-insert-fields (nreverse field-list-rv))))



(defun treadle-dbg-insert-component (wid)
   ""
   (let* 
      (
	 (sym (widget-get wid :value))
	 (v (symbol-value sym))
	 (perm-props (treadle-dbg-get-perm-props (symbol-name sym))))
      (treadle-dbg-insert-component-aux v perm-props)))

'
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
	       (treadle-dbg-get-perm-props (symbol-name sym))
	       firrtl-dbg-value-end-column)
	    " "
	    (firrtl-dbg-type-fmt
	       v
	       firrtl-dbg-type-end-column)))))
'
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
	       (treadle-dbg-get-perm-props (symbol-name sym))
	       firrtl-dbg-value-end-column)
	    " "
	    (firrtl-dbg-type-fmt
	       v
	       firrtl-dbg-type-end-column)))))


'
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
	       (treadle-dbg-get-perm-props (symbol-name sym))
	       firrtl-dbg-value-end-column)
	    " "
	    (firrtl-dbg-type-fmt
	       v
	       firrtl-dbg-type-end-column)))))
'
(defun firrtl-dbg-insert-register-component (wid)
   "Insert a register component"
   (let* 
      (
	 (sym (widget-get wid :value))
	 (v (symbol-value sym))
	 (perm-props
	    (treadle-dbg-get-perm-props (symbol-name sym))))
      
      (firrtl-dbg-insert-fields
	 (list
	    (list (firrtl-dbg-register-full-name v)
	       nil
	       firrtl-dbg-value-column)
	    (firrtl-dbg-field-fmt
	       (firrtl-dbg-register-current v)
	       perm-props
	       firrtl-dbg-value-end-column)
	    " -> "
	    (firrtl-dbg-field-fmt
	       (firrtl-dbg-register-next v)
	       perm-props
	       firrtl-dbg-next-value-end-column)
	    " "
	    (firrtl-dbg-type-fmt
	       v
	       firrtl-dbg-type-end-column)))))
'
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
'
(defun firrtl-dbg-tree-expand (tree)
   (or (widget-get tree :args)
      (let
	 ((alist (widget-get (tree-widget-node tree) :value)))
	 (mapcar #'firrtl-dbg-tree-widget alist))))

(defun treadle-dbg-tree-widget (cell)
   (let ()
      (if (second cell)
	 `(tree-widget
	     :node (push-button
		      :value ,(cddr cell)
		      :tag ,(car cell)
		      :format "%[%t%]\n"
		      ;; Nothing to do yet for inner nodes
		      :alt-action ,#'ignore)
	     :dynargs treadle-dbg-tree-expand)
	 (let*
	    ((sym (cddr cell)))
	    `(const
		:format "%v\n"
		:value ,sym
		:value-create ,#'treadle-dbg-insert-component
		;; UPDATE ME
		:alt-action ,#'firrtl-dbg-edit-properties)))))

(defun treadle-dbg-tree-expand (tree)
   (or (widget-get tree :args)
      (let
	 ((alist (widget-get (tree-widget-node tree) :value)))
	 (mapcar #'treadle-dbg-tree-widget alist))))


(defun treadle-dbg-create-widgets ()
   '  ;; REENABLE ME
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer
	 "Creating the widgets only makes sense in a circuit buffer"))

   (widget-insert "FIRRTL debugger interface\n\n")
   '
   (setq firrtl-dbg-widget-of-step-num
      (widget-create 'const
	 :value treadle-dbg-current-step
	 :format "Step %v"))
   
   (widget-insert " ")
   '
   (setq firrtl-dbg-widget-of-freshness
      (widget-create 'const
	 :value treadle-dbg-current-freshness
	 :format "(%v)"))

   (widget-insert "\n\n")

   '
   (widget-create 'push-button
      :notify (lambda (&rest ignore)
		 (firrtl-dbg-step-circuit))
      "Step")

   (widget-insert "   ")
   '
   (widget-create 'push-button
      :notify (lambda (&rest ignore)
		 (unless (eq firrtl-dbg-current-buffer-type 'main)
		    (treadle-dbg-complain-bad-buffer
		       "Rebuilding the widgets only makes sense in a circuit buffer"))

		 (let
		    ((inhibit-read-only t))
		    (erase-buffer))
		 (firrtl-dbg-create-widgets))
      "Rebuild buffer")

   (widget-insert "   ")
   '
   (widget-create 'push-button
      :notify
      (lambda (&rest ignore)
	 (unless (eq firrtl-dbg-current-buffer-type 'main)
	    (treadle-dbg-complain-bad-buffer))
	 (firrtl-dbg-shutdown))
      "Done")
   
   ;; IMPROVE ME: Add other buttons: Reset, (Done), Poison, Randomize,
   ;; Start/stop recording script, etc
   (widget-insert "\n\n")

   (widget-apply-action
      (widget-create (treadle-dbg-tree-widget
			(cons "root" treadle-dbg-subname-tree))))
   (if (require 'tree-mode nil t)
      (tree-minor-mode t)
      (widget-insert "\n\n"))
   (use-local-map widget-keymap)
   '
   (local-set-key "\M-\r"
      #'firrtl-dbg-do-alt-interaction))

'
(defun firrtl-dbg-create-widgets ()
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer
	 "Creating the widgets only makes sense in a circuit buffer"))

   (widget-insert "FIRRTL debugger interface\n\n")
   (setq firrtl-dbg-widget-of-step-num
      (widget-create 'const
	 :value treadle-dbg-current-step
	 :format "Step %v"))
   
   (widget-insert " ")
   (setq firrtl-dbg-widget-of-freshness
      (widget-create 'const
	 :value treadle-dbg-current-freshness
	 :format "(%v)"))

   (widget-insert "\n\n")
   
   (widget-create 'push-button
      :notify (lambda (&rest ignore)
		 (firrtl-dbg-step-circuit))
      "Step")

   (widget-insert "   ")

   (widget-create 'push-button
      :notify (lambda (&rest ignore)
		 (unless (eq firrtl-dbg-current-buffer-type 'main)
		    (treadle-dbg-complain-bad-buffer
		       "Rebuilding the widgets only makes sense in a circuit buffer"))

		 (let
		    ((inhibit-read-only t))
		    (erase-buffer))
		 (firrtl-dbg-create-widgets))
      "Rebuild buffer")

   (widget-insert "   ")

   (widget-create 'push-button
      :notify
      (lambda (&rest ignore)
	 (unless (eq firrtl-dbg-current-buffer-type 'main)
	    (treadle-dbg-complain-bad-buffer))
	 (firrtl-dbg-shutdown))
      "Done")
   
   ;; IMPROVE ME: Add other buttons: Reset, (Done), Poison, Randomize,
   ;; Start/stop recording script, etc
   (widget-insert "\n\n")

   (widget-apply-action
      (widget-create (firrtl-dbg-tree-widget
			(cons "root" treadle-dbg-subname-tree))))
   (if (require 'tree-mode nil t)
      (tree-minor-mode t)
      (widget-insert "\n\n"))
   (use-local-map widget-keymap)
   (local-set-key "\M-\r"
      #'firrtl-dbg-do-alt-interaction))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADAPT ME
'
(defun firrtl-dbg-edit-properties (widget &optional event)
   "Edit the properties of a component symbol"

   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer
	 "Calling edit-properties only makes sense in the main buffer, although it creates a customize buffer"))

   (let* 
      (
	 (sym (widget-get widget :value))
	 (name (symbol-name sym))
	 (perm-sym-soft
	    (intern-soft name treadle-dbg-obarray-perm-props))
	 (perm-sym
	    (or perm-sym-soft
	       (intern name treadle-dbg-obarray-perm-props))))

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
	    (setq treadle-dbg-main-buffer main-buf)

	    (fset (make-local-variable 'Custom-save)
	       #'firrtl-dbg-save-perms)
	    (set (make-local-variable 'custom-variable-menu)
	       firrtl-dbg-custom-variable-menu))
	  
	 (pop-to-buffer-same-window buf))))

;; ADAPT ME
'

(defun firrtl-dbg-copy-alist-to-perms ()
   ""
   
   (interactive)
   (dolist (cell treadle-dbg-perm-props-alist)
      (let* 
	 ((name (car cell))
	    (value (cdr cell))
	    (sym (intern name treadle-dbg-obarray-perm-props)))
	 (set sym value))))


;; ADAPT ME
'

(defun firrtl-dbg-copy-perms-to-alist ()
   ""

   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer
	 "Copying to the perm alist only makes sense in the main buffer"))

   (setq treadle-dbg-perm-props-alist '())
   (mapatoms
      #'(lambda (sym)
	   (when sym
	      (push
		 (cons (symbol-name sym) (symbol-value sym))
		 treadle-dbg-perm-props-alist)))
      treadle-dbg-obarray-perm-props))

;; ADAPT ME
'

(defun firrtl-dbg-save-perms (&rest ignore)
   ""
   ;; This takes the place of Custom-save

   ;; CHECK ME: Do we need to copy perms to treadle-dbg-perm-props-alist?

   ;; Quick&dirty: Just save treadle-dbg-perm-props-alist.  We don't
   ;; save firrtl-dbg-custom-enums because that's useful globally
   ;; (though providing both local and global would be nice).  We
   ;; don't try to track what's dirty, nor treat an extensible set of
   ;; variables
   (add-dir-local-variable 'firrtl-dbg-mode
      'treadle-dbg-perm-props-alist
      treadle-dbg-perm-props-alist))


(defun treadle-dbg-get-perm-props (str)
   "Get the permanent props for the component named STR.
Return nil if component has no permanent props."
   
   (let*
      (
	 (perm-prop-sym
	    (intern-soft str treadle-dbg-obarray-perm-props)))
      (if perm-prop-sym (symbol-value perm-prop-sym) nil)))

;; ADAPT ME
'

(defun firrtl-dbg-custom-variable-save (widget)
   "Save value of variable edited by widget WIDGET."
   (custom-variable-mark-to-save widget)
   (save-excursion
      (let* 
	 ((sym (widget-get widget :value)))

	 ;; Customize buffer knows a particular widgets buffer
	 (with-current-buffer treadle-dbg-main-buffer
	    (unless (eq firrtl-dbg-current-buffer-type 'main)
	       (treadle-dbg-complain-bad-buffer))

	    ;; Copy this sym to treadle-dbg-perm-props-alist
	    (setq treadle-dbg-perm-props-alist
	       (cons
		  (cons (symbol-name sym) (symbol-value sym))
		  (delete-if
		     #'(lambda (a)
			  (string-equal (first a) (symbol-name sym)))
		     treadle-dbg-perm-props-alist)))
	    ;; IMPROVE ME: Nice to save the file automatically and not
	    ;; necessarily see the buffer in a window.
	    (add-dir-local-variable 'firrtl-dbg-mode
	       'treadle-dbg-perm-props-alist
	       treadle-dbg-perm-props-alist))))
   
   (custom-variable-state-set-and-redraw widget))
;; ADAPT ME
'

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
	    (push entry our-menu)))
      (nreverse our-menu)))

;; ADAPT ME
'

;; Needs to be after firrtl-dbg-make-custom-variable-menu
(defconst firrtl-dbg-custom-variable-menu
   (firrtl-dbg-make-custom-variable-menu)
   "" )
;; ADAPT ME
'

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
;; ADAPT ME
'

(defun firrtl-dbg-step-circuit ()
   "Step the circuit"
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   (widget-value-set
      firrtl-dbg-widget-of-freshness
      "Stepping")

   (when treadle-dbg-writing-script-p
      (push '(step) treadle-dbg-current-script-rv))

   (firrtl-dbg-step-circuit-low)
   (firrtl-dbg-show-circuit-low))

;; ADAPT ME
'

(defun firrtl-dbg-record-spurious-response-lines (str step-num)
   ""
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))

   (let
      ((spl (split-string str "\n"))
	 (collected-lines '()))
      
      (dolist (line spl)
	 (cond
	    ;; Blank line.  Ignore it.
	    ((string-match "^[ \t]*$" line))

	    ;; The prompt line.  Ignore it.
	    ((string-match firrtl-dbg-prompt-line-regexp line))
	    
	    (t
	       (push line collected-lines))))
      (let
	 ((line-data
	     (list step-num (nreverse collected-lines))))
	 (push line-data treadle-dbg-spurious-lines))))


;; ADAPT ME
'

(defun firrtl-dbg-step-circuit-low ()
   "Step the circuit"

   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   (tq-enqueue treadle-dbg-tq
      "step\n"
      treadle-dbg-tq-regexp
      (list (current-buffer))
      #'(lambda (data str)
	   (with-current-buffer (first data)
	      (firrtl-dbg-record-spurious-response-lines
		 str treadle-dbg-current-step)
	      (incf treadle-dbg-current-step)))
      
      t))
;; ADAPT ME
'

(defun firrtl-dbg-show-circuit-low ()
   "Get the current component values"

   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   (tq-enqueue treadle-dbg-tq
      "show\n"
      treadle-dbg-tq-regexp
      (list (current-buffer))
      #'(lambda (data str)
	   (with-current-buffer (first data)
	      ;; IMPROVE ME: At some point change all states of
	      ;; set-by-user-now to set-by-user-earlier.  For
	      ;; non-inputs, figure out whether it changed since last
	      ;; time.
	      (firrtl-dbg-build-data str)
	      (firrtl-dbg-redraw-widgets)))
      t))
;; ADAPT ME
'

(defun firrtl-dbg-remove-prompt-suffix (str)
   ""

   (let*
      (
	 (start-legit (string-match treadle-dbg-tq-prompt-string str)))
      (when (null start-legit)
	 (error "No FIRRTL prompt found"))
      (substring str 0 start-legit)))
;; ADAPT ME
'

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
;; Superseded in Treadle
'

(defun firrtl-dbg-init-component-type (name)
   "Set the type of component NAME according to the REPL"

   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   
   (let*
      ((command (concat "type " name "\n")))
      (tq-enqueue treadle-dbg-tq command treadle-dbg-tq-regexp
	 (list (current-buffer) name)
	 #'(lambda (data str)
	      (with-current-buffer (first data)
		 (unless (eq firrtl-dbg-current-buffer-type 'main)
		    (treadle-dbg-complain-bad-buffer))
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
			     type))))))
	 t)))

;; Superseded in Treadle
'
(defun firrtl-dbg-init-all-component-types ()
   ""
   
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   (mapatoms
      #'(lambda (sym)
	   (when sym
	      (firrtl-dbg-init-component-type (symbol-name sym))))
      firrtl-dbg-obarray))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Updating widgets due to new "show"

(defun treadle-dbg-for-all-buttons (proc)
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

;; ADAPT ME
'
(defun firrtl-dbg-redraw-widgets ()
   ""
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   (widget-value-set
      firrtl-dbg-widget-of-step-num
      treadle-dbg-current-step)
   (widget-value-set
      firrtl-dbg-widget-of-freshness
      treadle-dbg-current-freshness)

   (treadle-dbg-for-all-buttons
      #'(lambda (widget)
	   (let* 
	      ((widget (widget-get widget :node)))
	      (when (widget-get widget :value)
		 ;; This forces a redraw
		 (widget-value-set widget
		    (widget-value widget)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADAPT ME
'
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
	 (t (error "Not a boolean")))))

;; ADAPT ME
'
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

;; ADAPT ME
'
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

;; ADAPT ME
'
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


;; ADAPT ME
'
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

;; ADAPT ME
'
(defun firrtl-dbg-poke-value (sym new-val
				&optional extra-proc extra-data)
   "Poke NEW-VAL into the component named by SYM
Record the new value.  If EXTRA-PROC is non-nil, call it with extra-data."
   
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))

   (let* 
      (  
	 (component (symbol-value sym))
	 (component-name (firrtl-dbg-input-full-name component))
	 (current (firrtl-dbg-input-current component))
	 (msg (concat "poke " component-name " "
		 (number-to-string new-val) "\n")))
      
      ;; IMPROVE ME:  Pre-filter inputs so we don't get errors here.
      (tq-enqueue treadle-dbg-tq
	 msg
	 treadle-dbg-tq-regexp
	 (list current new-val extra-proc extra-data)
	 #'(lambda (data str)
	      (let* 
		 ((had-problem
		     (treadle-dbg-parse-response-maybe-complain str))
		    (current (first data))
		    (new-val (second data))
		    (extra-proc (third data))
		    (extra-data (fourth data)))

		 (unless had-problem
		    ;; Set the component's value to that.
		    (setf (firrtl-dbg-value-v current) new-val)
		    (setf (firrtl-dbg-value-state current) 'set-by-user-now)

		    (when extra-proc
		       (apply extra-proc extra-data)))))
	 t)))

;; ADAPT ME
'
(defun firrtl-dbg-do-integer-edit&poke (widget widget-again &optional event)
   ""
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
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
	    (treadle-dbg-get-perm-props (symbol-name sym)))
	 (new-val (firrtl-dbg-read-new-val
		     (format "New value for %s: " component-name)
		     current
		     perm-props))
	 (msg (concat "poke " component-name " "
		 (number-to-string new-val) "\n")))

      (when treadle-dbg-writing-script-p
	 (push
	    `(poke ,component-name ,new-val)
	    treadle-dbg-current-script-rv))
      (setq treadle-dbg-current-freshness "STALE")
      (widget-value-set
	 firrtl-dbg-widget-of-freshness
	 "STALE")

      ;; IMPROVE ME:  Pre-filter inputs so we don't get errors here.
      (tq-enqueue treadle-dbg-tq
	 msg
	 treadle-dbg-tq-regexp
	 (list current widget new-val (current-buffer))
	 #'(lambda (data str)
	      (let* 
		 ((had-problem
		     (treadle-dbg-parse-response-maybe-complain str))
		    (current (first data))
		    (widget (second data))
		    (new-val (third data))
		    (widgets-buffer (fourth data)))

		 (unless had-problem
		    ;; Set the component's value to that.
		    (setf (firrtl-dbg-value-v current) new-val)

		    (setf (firrtl-dbg-value-state current) 'set-by-user-now)
		    (with-current-buffer widgets-buffer
		       (widget-value-set widget (widget-value widget))))))
	 t)))

;; ADAPT ME
'
(defun firrtl-dbg-run-script (script)
   "Run SCRIPT.
Script should be a list whose entries are in on of the forms:
 (poke component-name-string val)
 (step)"
   
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))

   (widget-value-set
      firrtl-dbg-widget-of-freshness
      "Running script")

   (dolist (line script)
      (case (first line)
	 (poke
	    (firrtl-dbg-poke-value
	       (intern (second line) firrtl-dbg-obarray)
	       (third line)))
	 
	 (step
	    (firrtl-dbg-step-circuit-low))))
   
   ;; All done, now redisplay everything.
   (firrtl-dbg-show-circuit-low))



;; Examples:
;; (firrtl-dbg-run-script
;;    '((poke "io_value1" 4)
;;        (poke "io_value2" 12)))

;; (firrtl-dbg-run-script
;;    '((step)))

;; ADAPT ME
'
(defun firrtl-dbg-start-recording-script ()
   ""

   (interactive)
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   (setq treadle-dbg-current-script-rv '())
   (setq treadle-dbg-writing-script-p t))

;; ADAPT ME
'
(defun firrtl-dbg-stop-recording-script ()
   ""
   
   (interactive)
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   (setq treadle-dbg-writing-script-p nil))

;; ADAPT ME
'
(defun firrtl-dbg-get-script ()
   ""
   
   (interactive)
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   (with-output-to-temp-buffer "*FIRRTL script*"
      (princ ";;User-generated script\n")
      (princ ";;Call as '(firrtl-dbg-run-script SCRIPT)'\n")
      (princ "\n")
      (prin1
	 (reverse treadle-dbg-current-script-rv))))


(defun treadle-dbg-parse-response-maybe-complain (str)
   "Return non-nil if str caused an error message"
   
   (let*
      (
	 (legit-rx (concat " *" treadle-dbg-tq-prompt-string))
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

(defstruct (treadle-dbg-timer-data (:type list))
   ""
   seconds-to-wait
   timer)


(defun treadle-dbg-call-until-done-w/timeout
   (num-seconds proc args &optional timed-out-proc timed-out-args)
   "
PROC should return non-nil if it has finished its work"
   (let
      ((data (make-treadle-dbg-timer-data :seconds-to-wait num-seconds)))
      (setf (treadle-dbg-timer-data-timer data)
	 (run-at-time t 1
	    #'(lambda (data proc args timed-out-proc timed-out-args)
		 ;; Manage timeout
		 (if (<= (treadle-dbg-timer-data-seconds-to-wait data) 0)
		    (progn
		       (cancel-timer (treadle-dbg-timer-data-timer data))
		       (when timed-out-proc
			  (apply timed-out-proc timed-out-args)))
		    (let
		       ((done (apply proc args)))
		       (if done
			  (cancel-timer (treadle-dbg-timer-data-timer data))
			  (decf (treadle-dbg-timer-data-seconds-to-wait data))))))
	    data
	    proc args
	    timed-out-proc timed-out-args))))

(defun treadle-dbg-process-is-ready-p (process)
   "True if the treadle-dbg process is ready, meaning that it has arrived at its initial prompt.  This may take a while."
   (with-current-buffer (process-buffer process)
      (goto-char (point-min))
      (search-forward treadle-dbg-tq-prompt-string nil t)))
;; ADAPT ME
'
(defun firrtl-dbg-initial-load ()
   ""
   (unless (eq firrtl-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   ;; 'firrtl-dbg-init-all-component-types' doesn't immediately show
   ;; results because it is waiting for the FIRRTL REPL to answer,
   ;; many times.  Could solve this with timers and dirty flags, but
   ;; it's not a serious problem.
   (tq-enqueue treadle-dbg-tq "show\n" treadle-dbg-tq-regexp
      (list (current-buffer))
      #'(lambda (data str)
	   (with-current-buffer (first data)
	      (unless (eq firrtl-dbg-current-buffer-type 'main)
		 (treadle-dbg-complain-bad-buffer))
	      (firrtl-dbg-build-data str)
	      (firrtl-dbg-init-all-component-types)
	      (firrtl-dbg-create-widgets)))
      t))

(define-derived-mode treadle-dbg-mode
   special-mode "Treadle-Dbg"
   "Major mode for Treadle debugger interface"
   :group 'treadle-dbg
   (progn
      (set-keymap-parent treadle-dbg-mode-map widget-keymap)
      (define-key treadle-dbg-mode-map  "\M-\r"
	 #'firrtl-dbg-do-alt-interaction)))


(defun treadle-dbg-complain-bad-buffer (&optional msg)
   ""
   (error (or msg "This operation only makes sense in main buffer")))

'
(defmacro firrtl-dbg-local-defvar (name value docstring)
   "Define VAR as a buffer-local variable with default value VAL.
This is different than defvar-local in that it doesn't define the variable in other buffers."
   
   `(progn
       (set (make-local-variable ',name) ,value)
       (put ',name 'variable-documentation ,docstring)))
;; ADAPT ME
'
(defun firrtl-dbg (working-directory repl-launch-command)
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
	 (treadle-dbg-mode)
	 (setq default-directory working-directory)
	 ;; Set up most of the local variables.  Some are set further
	 ;; down as their objects are created.
	 (set (make-local-variable 'firrtl-dbg-current-buffer-type)
	    'main)

	 (setq treadle-dbg-process-buffer
	    (generate-new-buffer treadle-dbg-process-buffer-name))

	 (with-current-buffer treadle-dbg-process-buffer
	    (setq default-directory working-directory)
	    (setq treadle-dbg-main-buffer main-buf))

	 (setq treadle-dbg-process
	    (let ((default-directory working-directory))
	       (start-process
		  treadle-dbg-process-name
		  treadle-dbg-process-buffer
		  firrtl-dbg-executable
		  ;; Quoting this string with shell-quote-argument
		  ;; actually messes us up.
		  repl-launch-command)))

	 (hack-dir-local-variables-non-file-buffer)
	 (firrtl-dbg-copy-alist-to-perms)
	 
	 (treadle-dbg-call-until-done-w/timeout
	    40
	    #'(lambda (process main-buf)
		 (when
		    (treadle-dbg-process-is-ready-p process)
		    (message "Debugger process is ready")
		    (let* 
		       ((tq (tq-create process)))
		       (with-current-buffer main-buf
			  (setq treadle-dbg-tq tq)
			  (firrtl-dbg-initial-load)))
		    (pop-to-buffer main-buf)
		    ;; Indicate that we have succeeded
		    t))
	    (list treadle-dbg-process main-buf)
	    #'(lambda ()
		 (message "Debugger process timed out"))
	    '()))))




;;;_. Footers
;;;_ , Provides

(provide 'treadle-dbg)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: emacs-lisp
;;;_  + End:

;;;_ , End
;;; treadle-dbg.el ends here
