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
;; treadle-dbg-directory-history so that your current project pops up
;; at the top of the history lists when calling firrtl-dbg.

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
(require 'pp)
(require 'subr-x)
(require 'tinydb/asynq)
(require 'tinydb/persist)
(require 'tq)
(require 'tree-widget)
;;;_. Body



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
   forced-p ;; Whether it is currently forced.  Maybe instead a
	    ;; current state symbol distinguishing normal input
	    ;; setting from "forced" from set-by-script.
   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations and constants

(defgroup treadle-dbg nil "Customizations for Firrtl-dbg"
   :group 'applications)

(defface treadle-dbg-face-value-default nil
   "The default face for values"
   :group 'treadle-dbg)


(defface treadle-dbg-face-value-input-unset '((t :background "gray"))
   "The face for input values we never set"
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
(defcustom treadle-dbg-custom-enums
   '()
   "Customization for enumerated values"
   ;; The first string is the name of the enum.  The repeated strings
   ;; are the enumerated values.  We don't try to support arbitrary
   ;; starting points etc.
   :type '(repeat
	     (group string
		(repeat string)))
   :group 'treadle-dbg)

;; IMPROVE ME:  Make this local
(defcustom treadle-dbg-custom-sorting
   '(("^io" 50)
       ("^reset$" 2000)
       ("^clock$" 2000))
   "List controlling which components are placed earlier or later in the expanding tree.  Each element is (REGEXP Integer)"
   :type '(repeat
	     (group
		regexp
		integer))
   :group 'treadle-dbg)

(defconst treadle-dbg-component-perm-spec
   '(choice
       (group
	  (const decimal))
       (group
	  (const boolean))
       (group
	  (const hexadecimal)
	  :tag "Hexadecimal")
       (group
	  (const enum)
	  (string))
       (group
	  (const base)
	  (integer)
	  :tag "In a given base"))
   
   "Customize spec that applies to all components" )


(defconst treadle-dbg-component-perm-standard-value
   '(decimal)
   "The standard value for components perm-spec")

;; Doesn't tag the compile-process nor firrlt source buffers
(defvar-local treadle-dbg-current-buffer-type nil
   "What type of buffer the current buffer is.  
Possible values are (nil 'main 'custom 'process 'perms-file).
Local in the relevant buffers." )

(defconst treadle-dbg-obarray-default-size 257
   "The default size of an obarray" )

;;;;;;;;;;;;;;;;;;;;
;; Launching Treadle

(defcustom treadle-dbg-directory-history
   '()
   "History list of working directories.  Put the directory that you would run sbt in for your project on this list.  Then 'treadle-dbg' will see it as a history item"
   :type '(repeat directory)
   :group 'treadle-dbg)

(defconst treadle-dbg-repl-launch-command
   "runMain treadle.TreadleRepl"
   "" )

;; IMPROVE ME: Allow a local version to override this if non-nil
(defcustom treadle-dbg-fir-file-location
   "fir-file-for-treadle-dbg.fir"
   "Location of FIRRTL source file.
If non-nil, enables compiling"
   :type
   '(choice
       (const nil :tag "Disabled")
       (string :tag "File name"))
   :group 'treadle-dbg)


(defcustom treadle-dbg-how-to-find-fir-file
   'always-use-latest-test
   "How launcher should find the FIR file"
   :type
   '(choice
       (const always-use-latest-test
	  :tag "Automatically use the FIRRTL of the latest test")
       (const manual :tag "Always specify it interactively")
       (const use-custom-file
	  :tag "Use 'treadle-dbg-fir-file-location'")
       ;; Not ready yet.
       ;; (const recompile-custom-file
       ;; 	  :tag "Use 'treadle-dbg-fir-file-location', recompiling it every time")
       )
   :group 'treadle-dbg)


(defvar-local treadle-dbg-recompile-base-command
   "test:runMain triggerPulses.indirect3.TriggerPulses.CompileDummy"
   ;; nil
   "Command in sbt to recompile FIRRTL.  The output filename will be appended to it")

(defconst treadle-dbg-recompile-base-command-spec
   '(string :tag "Command in sbt to recompile FIRRTL")
   "Customize spec for treadle-dbg-recompile-base-command" )

;;;;;;;;;;;;;;;;;;;;
;;Configuration

(defcustom treadle-dbg-sbt-executable
   "sbt"
   "Name of the actual executable that helps launch the debugger REPL"
   :type 'string
   :group 'treadle-dbg)

(defcustom treadle-dbg-timeout
   40
   "How long to wait for the external process to start"
   :type 'integer
   :group 'treadle-dbg)

(defconst treadle-dbg-process-name
   "treadle-dbg-process"
   "Name for the process that communicates with the debugger REPL" )

(defconst treadle-dbg-process-buffer-name
   "*Treadle-dbg process*"
   "Name of the process buffer" )

(defconst treadle-dbg-compile-process-name
   "treadle-dbg-compile-process"
   "Name for the process that compiles FIRRTL files" )

(defconst treadle-dbg-compile-process-buffer-name
   "*Treadle-dbg compile process*"
   "Name of the compile process buffer when active" )

(defcustom treadle-dbg-perm-props-relative-filename
   "tread-dbg-perm-props"   
   "Relative name of the file to store perm properties in"
   :group 'treadle-dbg
   :type 'string)


;;;;;;;;;;;;;;;;;;;;
;; Regexps

(defcustom treadle-dbg-tq-prompt-string
   "treadle>>"
   "The REPL's prompt string"
   :type 'string
   :group 'treadle-dbg)

(defconst treadle-dbg-tq-regexp
   (concat ".*" treadle-dbg-tq-prompt-string " *")
   "Regexp matching any response from the REPL" )

(defconst treadle-dbg-prompt-line-regexp
   (concat "[0-9]* *" treadle-dbg-tq-prompt-string " *")
   "Regexp matching a bare prompt line from the REPL" )

(defconst treadle-dbg-prompt-line-regexp-leading-cr
   (concat "\n" treadle-dbg-prompt-line-regexp)
   "Regexp matching a bare prompt line from the REPL" )

;;;;;;;;;;;;;;;;;;;;
;; Print columns


(defconst treadle-dbg-value-column 15
   "Column that values should print at" )

(defconst treadle-dbg-value-end-column 25
   "Column that values should end at" )

(defconst treadle-dbg-next-value-begin-column
   (+ 4 treadle-dbg-value-end-column)
   "Column that values should end at" )

(defconst treadle-dbg-next-value-end-column
   (+ 20 treadle-dbg-next-value-begin-column)
   "Column that next-value should end at" )

(defconst treadle-dbg-type-end-column
   (+ 20 treadle-dbg-next-value-end-column)
   "Column that type should end at" )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Widgets
(defvar-local treadle-dbg-widget-of-step-num
   nil
   "Widget displaying the current step value")

(defvar-local treadle-dbg-widget-of-freshness
   nil
   "Widget displaying the current freshness")

(defvar-local treadle-dbg-widget-of-circuit-name
   nil
   "Widget displaying the name of the current circuit")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data items
(defvar-local treadle-dbg-spurious-lines
   '()
   "Spurious lines from stepping")

(defvar-local treadle-dbg-obarray
   (make-vector treadle-dbg-obarray-default-size nil)
   "Obarray that holds the current treadle data of FIRRTL components")

(defvar-local treadle-dbg-obarray-perm-props
   (make-vector treadle-dbg-obarray-default-size nil)
   "Obarray that holds data about components that persists between sessions")

(defvar-local treadle-dbg-subname-tree
   '()
   "The component-tree of the circuit.

Format: Each node is either:
  (subname-string t list-of-nodes)
  (subname-string nil . sym)"
   )

(defvar-local treadle-dbg-have-built-subname-tree
   nil
   "Whether the subname tree has been built yet")

(defvar-local treadle-dbg-current-step
   nil
   "The current step of the circuit")

(defvar-local treadle-dbg-current-freshness
   "NOT LOADED"
   "The current freshness of the circuit, as a string")

(defvar-local treadle-dbg-current-circuit-name
   ""
   "The name of the current circuit, as a string")

(defvar-local treadle-dbg-writing-script-p
   nil
   "Whether we are currently writing a script")

(defvar-local treadle-dbg-current-script-rv
   '()
   "The script that we are currently writing, in reverse order")

(defvar-local treadle-dbg-process-buffer
   nil
   "The buffer of the Treadle REPL process")

(defvar-local treadle-dbg-main-buffer
   nil
   "The main interaction buffer")

(defvar-local treadle-dbg-perm-props-buffer
   nil
   "The buffer holding a text representation of perm properties" )

(defvar-local treadle-dbg-tq
   nil
   "The treadle-dbg transaction queue")

(defvar-local treadle-dbg-process
   nil
   "The Treadle REPL process")



;;;;;;;;;;;;;;;;;;;;

(defun treadle-dbg-add-to-subname-tree (tree subname-list data)
   "
TREE should be '(list subtree...) or '(tag values...) where tag is one of the component struct tags.

DATA is the data to store, usually a symbol"

   ;; We indicate listness with t or nil.
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


;; (treadle-dbg-add-to-subname-tree '(t ("a" t ("b" nil my-data))) '("a" "b")
;;     'new-data)


;; (treadle-dbg-add-to-subname-tree '(t ("a" t ("b" nil my-data))) '("a" "c")
;;     'new-data)


;; (treadle-dbg-add-to-subname-tree '(t ("a" t ("b" nil my-data))) '("d" "b")
;;     'new-data)

(defun treadle-dbg-sort-as-subname-tree (tree)
   "
TREE should be '(list subtree...) or '(tag values...) where tag is one of the component struct tags.

Return a sorted version of it"
   (let* 
      ((new-children
	  (mapcar
	     #'(lambda (cell)
		  (if (cadr cell)
		     (list*
			(car cell)
			(cadr cell)
			(treadle-dbg-sort-as-subname-tree (cddr cell)))
		     cell))
	     
	     (cdr tree)))
	 (children-2
	    (sort new-children
	       #'(lambda (a b)
		    (let*
		       ((a-1 (car-safe a))
			  (b-1 (car-safe b)))
		       (if (numberp a-1)
			  (if (numberp b-1)
			     (< a-1 b-1)
			     t)
			  (if (numberp b-1)
			     nil
			     (string< a-1 b-1))))))))
      (cons (car tree) children-2)))





(defun treadle-dbg-split-component-name (str)
   ""
   (if
      (eql (elt str 0) ?/)
      (cons "/" (split-string (substring str 1) "[._]+"))
      (split-string str "[._]+")))

;; (treadle-dbg-split-component-name "io_a.b")
;; (treadle-dbg-split-component-name "/print0")

(defstruct treadle-dbg-state-entry
   ""
   full-name
   split-name ;; Remove this
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
   ;; IMPROVE ME:  Use ignore-errors and let caller abort on nil.
   (string-match "^\\([^ ]+\\) +\\([0-9]+\\)" str)
   
   (let*
      (
	 (name-plus (match-string 1 str))
	 (value (string-to-number (match-string 2 str)))
	 (spl (split-string name-plus "/"))
	 (full-name (car spl))
	 (qualifiers nil))
      ;; It's possible for it to begin with "/" eg for "/print0 0"
      ;; which is different than splitting off "/in" etc.
      (when (string-blank-p full-name)
	 (setq full-name name-plus))

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
	 :qualifiers qualifiers
	 :value value)))



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

(defun treadle-dbg-get-display-priority (full-name)
   ""
   
   (let*
      ((num 999))
      (dolist (cell treadle-dbg-custom-sorting)
	 (when (string-match (first cell) full-name)
	    (setq num (second cell))))
      num))

(defun treadle-dbg-mutate-subname-tree (full-name data)
   ""
   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer
	 "Objects are only available in the main buffer"))

   (let*
      ((num-prefix (treadle-dbg-get-display-priority full-name))
	 (split-name
	    (cons
	       num-prefix
	       (treadle-dbg-split-component-name full-name))))
      (setq
	 treadle-dbg-subname-tree
	 (treadle-dbg-add-to-subname-tree treadle-dbg-subname-tree
	    split-name
	    data))))

(defun treadle-dbg-add-object (full-name proc-mutate)
   ""
   (unless (eq treadle-dbg-current-buffer-type 'main)
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

(defun treadle-dbg-set-data-aux (state-string mutator)
   "
MUTATOR takes two arguments.  First is a treadle-dbg-component, second is a treadle-dbg-state-entry"
   (let*
      ((spl (split-string state-string "\n")))
      (dolist (line spl)
	 (unless (string-blank-p line)
	    ;; FIX ME: Handle the "/printX" components correctly, and
	    ;; any other slashes.
	    (let* 
	       ((e (treadle-dbg-str->state-entry line))
		  (full-name (treadle-dbg-state-entry-full-name e)))
	       (when (string-blank-p full-name)
		  (message "Got a blank name in line %s") line)
	       (unless (string-blank-p full-name)
		  (treadle-dbg-add-object
		     full-name
		     #'(lambda (component)
			  (funcall mutator component e)))))))))

;; Processes the return string from "show state"
(defun treadle-dbg-record-state (state-string)
   "Set the data of components from the result of Treadle 'show state'"
   (treadle-dbg-set-data-aux
      state-string
      #'treadle-dbg-mutate-component-value)
   (setq treadle-dbg-have-built-subname-tree t))


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

(defstruct (symbol-record-strings (:type list))
   ""
   trunc-name type-str width-str source-str value-str
   ;; Not using: bin slots index depend
   )
(defun treadle-dbg-test-second-symbol-line (name symbol-string)
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

(defun treadle-dbg-record-symbol-info (name symbol-string)
   ""
   (let*
      ((spl (split-string symbol-string "\n"))
	 (found nil))
      (unless (string-match-p
		 "Name +Bin +Type +Width +Slots +Index +Depend +Info"
		 (car spl))
	 (error "This symbol report is not in the expected format: %S"
	    (car spl)))
      
      (dolist (line (cdr spl))
	 (cond
	    ;; If we've already found it we're done
	    (found)
	    ;; The prompt.  Nothing to do.
	    ((string-match-p treadle-dbg-prompt-line-regexp line))
	    ;; Blank line.  Nothing to do.
	    ((string-blank-p line))
	    (t
	       ;; We don't try to reject foo/in etc here because they
	       ;; are filtered out before here by regexp ^foo$
	       (let* 
		  ((info (treadle-dbg-symbol-string->struct line)))
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
				      (t (message
					    "Unrecognized type-str '%s' from line %s"
					    type-str
					    line))))
				(width
				   (string-to-number
				      (symbol-record-strings-width-str info))))
			     (unless
				(eql (treadle-dbg-component-current component)
				   (string-to-number value-str))
				(message "Values do not match! %s != %s"
				   (treadle-dbg-component-current component)
				   (string-to-number value-str)))
			     (setf
				(treadle-dbg-component-width component)
				width)
			     (setf (treadle-dbg-component-signed-p component)
				signed-p)
			     (setf (treadle-dbg-component-source component)
				source-str))))
		  (setq found t)))))))

(defun treadle-dbg-clear ()
   "Clear all the values; ready to start again"
   (interactive)

   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer
	 "Clearing the data only makes sense in a circuit buffer"))

   (setq treadle-dbg-have-built-subname-tree nil)
   (setq treadle-dbg-subname-tree '())
   (setq treadle-dbg-obarray
      (make-vector treadle-dbg-obarray-default-size nil))
   (setq treadle-dbg-spurious-lines '()))


(defun treadle-dbg-shutdown ()
   ""
   
   (interactive)

   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer
	 "Shutting down only makes sense in a circuit buffer"))

   (treadle-dbg-clear)
   
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

(defun treadle-dbg-enum-string (fmt index)
   ""
   
   (let*
      (  (key (second fmt))
	 (found (assoc key treadle-dbg-custom-enums)))
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


(defun treadle-dbg-insert-component-aux (component perm-props)
   ""
   (let* 
      ((width-string
	  (if (treadle-dbg-component-width component)
	     (number-to-string (treadle-dbg-component-width component))
	     "??"))
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
		  'treadle-dbg-face-value-default)))
	 (field-list-rv '()))
      (push
	 (list
	    (treadle-dbg-component-full-name component)
	    nil)
	 field-list-rv)
      (push
	 (list 'to-col treadle-dbg-value-column)
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
	 (list 'to-col (1- treadle-dbg-value-end-column) face-of-current)
	 field-list-rv)
      (push
	 (list 'to-col treadle-dbg-value-end-column nil)
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

(defun treadle-dbg-tree-widget (cell)
   (let ()
      (if (second cell)
	 (let*
	    (  (start-open-p nil)
	       (raw-tag (car cell))
	       (format "%[%t%]\n")
	       (tag
		  (cond
		     ((numberp raw-tag)
			(setq format "%t\n")
			(when (< raw-tag 100)
			   (setq start-open-p t))
			(concat "[" (number-to-string raw-tag) "]"))
		     ((stringp raw-tag) raw-tag)
		     (t "?"))))
	    
	    `(tree-widget
		:node (push-button
			 :value ,(cddr cell)
			 :tag ,tag
			 :format ,format
			 ;; Nothing to do yet for inner nodes
			 :alt-action ,#'ignore)
		,@(if start-open-p '(:open t) '())
		:dynargs treadle-dbg-tree-expand))
	 (let*
	    ((sym (cddr cell)))
	    `(const
		:format "%v\n"
		:value ,sym
		:value-create ,#'treadle-dbg-insert-component
		:alt-action ,#'treadle-dbg-edit-properties
		:notify ,#'treadle-dbg-do-integer-edit&poke)))))

(defun treadle-dbg-tree-expand (tree)
   (or (widget-get tree :args)
      (let
	 ((alist (widget-get (tree-widget-node tree) :value)))
	 (mapcar #'treadle-dbg-tree-widget alist))))


(defun treadle-dbg-create-widgets ()
   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer
	 "Creating the widgets only makes sense in a circuit buffer"))

   (widget-insert "Treadle debugger interface\n")

   (setq treadle-dbg-widget-of-circuit-name
      (widget-create 'const
	 :value treadle-dbg-current-circuit-name
	 :format "%v"))
   
   (widget-insert "\n\n")

   (setq treadle-dbg-widget-of-step-num
      (widget-create 'const
	 :value treadle-dbg-current-step
	 :format "Step %v"))
   
   (widget-insert " ")

   (setq treadle-dbg-widget-of-freshness
      (widget-create 'const
	 :value treadle-dbg-current-freshness
	 :format "(%v)"))

   (widget-insert "\n\n")

   (widget-create 'push-button
      :notify (lambda (&rest ignore)
		 (treadle-dbg-step-circuit))
      "Step")

   (widget-insert "   ")

   (widget-create 'push-button
      :notify (lambda (&rest ignore)
		 (unless (eq treadle-dbg-current-buffer-type 'main)
		    (treadle-dbg-complain-bad-buffer
		       "Rebuilding the widgets only makes sense in a circuit buffer"))

		 (let
		    ((inhibit-read-only t))
		    (erase-buffer))
		 (treadle-dbg-create-widgets))
      "Rebuild buffer")

   (widget-insert "   ")

   (widget-create 'push-button
      :notify (lambda (&rest ignore)
		 (treadle-dbg-reset-circuit))
      "Reset")

   (widget-insert "   ")

   (widget-create 'push-button
      :notify
      (lambda (&rest ignore)
	 (unless (eq treadle-dbg-current-buffer-type 'main)
	    (treadle-dbg-complain-bad-buffer))
	 (treadle-dbg-shutdown))
      "Done")
   
   (widget-insert "   ")

   (widget-create 'push-button
      :notify
      (lambda (&rest ignore)
	 (unless (eq treadle-dbg-current-buffer-type 'main)
	    (treadle-dbg-complain-bad-buffer))
	 (treadle-dbg-compile&restart))
      "Recompile & restart")
   
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

   (local-set-key "\M-\r"
      #'treadle-dbg-do-alt-interaction))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun treadle-dbg-edit-properties (widget &optional event)
   "Edit the properties of a component symbol"

   (unless (eq treadle-dbg-current-buffer-type 'main)
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
	    (list 'quote treadle-dbg-component-perm-standard-value)
	    "The format to display the component in"
	    :type treadle-dbg-component-perm-spec))

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
	       #'treadle-dbg-save-perms)
	    (set (make-local-variable 'custom-variable-menu)
	       treadle-dbg-custom-variable-menu))
	  
	 (pop-to-buffer-same-window buf))))

;;;;;;;;;;;;;;;;;;;;
;; Storing perm props

;; The perms buffer must be set up knowing treadle-dbg-main-buffer,
;; and treadle-dbg-write-perms-to-buffer will be in its
;; write-contents-functions

(defun treadle-dbg-get-perm-props-filename ()
   ""

   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))

   (concat default-directory
      treadle-dbg-perm-props-relative-filename))


;; Get all the saved props into treadle-dbg-obarray-perm-props
(defun treadle-dbg-load-perm-props-from-alist (alist)
   ""
   
   (dolist (cell alist)
      (let* 
	 ((name (car cell))
	    (value (cdr cell))
	    (sym (intern name treadle-dbg-obarray-perm-props)))
	 (set sym value))))

(defun treadle-dbg-read-object-from-buffer ()
   "Return the current buffer's data read as a lisp object."

   (unless (eq treadle-dbg-current-buffer-type 'perms-file)
      (treadle-dbg-complain-bad-buffer
	 "Function only makes sense in the perms buffer"))
   (if (eql (buffer-size) 0)
      nil
      (condition-case err
	 (progn
	    (goto-char (point-min))
	    (read (current-buffer)))
	 (error 
	    (message-box "Perm props file contained no data.")
	    '()))))

(defun treadle-dbg-write-perms-to-buffer ()
   ""
   (unless (eq treadle-dbg-current-buffer-type 'perms-file)
      (treadle-dbg-complain-bad-buffer
	 "Function only makes sense in the perms buffer"))
   ;; Get the perms as an alist.
   (let*
      ( (obj
	   (with-current-buffer treadle-dbg-main-buffer
	      (treadle-dbg-get-all-perms))))
      (erase-buffer)
      (insert (pp-to-string obj))
      nil))

(defun treadle-dbg-get-all-perms ()
   ""
   
   (list
      (cons 'perm-props (treadle-dbg-get-perm-props-as-alist))))

(defun treadle-dbg-load-all-perms (object)
   ""
   
   (treadle-dbg-load-perm-props-from-alist
      (cdr (assoc 'perm-props object))))


(defun treadle-dbg-get-perm-props-as-alist ()
   ""

   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer
	 "Copying to the perm alist only makes sense in the main buffer"))

   ;; IMPROVE ME: Maybe should only be saving the symbols' perm
   ;; customs, and not everything about them.
   (let
      ((alist '()))
      (mapatoms
	 #'(lambda (sym)
	      (when sym
		 (push
		    (cons (symbol-name sym) (symbol-value sym))
		    alist)))
	 treadle-dbg-obarray-perm-props)
      alist))


(defun treadle-dbg-save-perms (&rest ignore)
   ""
   ;; This takes the place of Custom-save, but just marks the perms
   ;; buffer dirty to be saved later.
   (with-current-buffer treadle-dbg-perm-props-buffer
      (restore-buffer-modified-p t)))

(defun treadle-dbg-get-perm-props (str)
   "Get the permanent props for the component named STR.
Return nil if component has no permanent props."
   
   (let*
      (
	 (perm-prop-sym
	    (intern-soft str treadle-dbg-obarray-perm-props)))
      (if perm-prop-sym (symbol-value perm-prop-sym) nil)))


(defun treadle-dbg-custom-variable-save (widget)
   "Save value of variable edited by widget WIDGET."
   (custom-variable-mark-to-save widget)
   (with-current-buffer treadle-dbg-perm-props-buffer
      (restore-buffer-modified-p t))
   (custom-variable-state-set-and-redraw widget))

(defun treadle-dbg-make-custom-variable-menu ()
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
		       treadle-dbg-custom-variable-save
		       (lambda
			  (widget)
			  (memq
			     (widget-get widget :custom-state)
			     '(modified set changed rogue))))
		   i)))
	    (push entry our-menu)))
      (nreverse our-menu)))

;; Needs to be after treadle-dbg-make-custom-variable-menu
(defconst treadle-dbg-custom-variable-menu
   (treadle-dbg-make-custom-variable-menu)
   "" )


(defun treadle-dbg-do-alt-interaction (pos &optional event)
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

(defun treadle-dbg-step-circuit ()
   "Step the circuit"
   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   (widget-value-set
      treadle-dbg-widget-of-freshness
      "Stepping")

   (when treadle-dbg-writing-script-p
      (push '(step) treadle-dbg-current-script-rv))

   (treadle-dbg-step-circuit-low)
   (treadle-dbg-show-components
      "show state\n"
      #'treadle-dbg-record-state)
   (setq treadle-dbg-current-freshness "FRESH")
   (treadle-dbg-redraw-widgets))

(defun treadle-dbg-reset-circuit ()
   "Reset the circuit"
   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   (widget-value-set
      treadle-dbg-widget-of-freshness
      "Resetting")
   (setq treadle-dbg-writing-script-p nil)

   (treadle-dbg-reset-circuit-low)
   (mapatoms
      #'(lambda (sym)
	   (when sym
	      (let* 
		 ((component (symbol-value sym)))
		 (setf
		    (treadle-dbg-component-forced-p component)
		    nil))))
      treadle-dbg-obarray)

   (treadle-dbg-show-components
      "show state\n"
      #'treadle-dbg-record-state)
   (setq treadle-dbg-current-freshness "FRESH")
   (treadle-dbg-redraw-widgets))


(defun treadle-dbg-record-spurious-response-lines (str step-num)
   ""
   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))

   (let
      ((spl (split-string str "\n"))
	 (collected-lines '()))
      
      (dolist (line spl)
	 (cond
	    ;; Blank line.  Ignore it.
	    ((string-match "^[ \t]*$" line))

	    ;; The prompt line.  Ignore it.
	    ((string-match treadle-dbg-prompt-line-regexp line))
	    
	    (t
	       (push line collected-lines))))
      (let
	 ((line-data
	     (list step-num (nreverse collected-lines))))
	 (push line-data treadle-dbg-spurious-lines))))


(defun treadle-dbg-step-circuit-low ()
   "Step the circuit"

   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   (tq-enqueue treadle-dbg-tq
      "step\n"
      treadle-dbg-tq-regexp
      (list (current-buffer))
      #'(lambda (data str)
	   (with-current-buffer (first data)
	      (treadle-dbg-record-spurious-response-lines
		 str treadle-dbg-current-step)
	      (incf treadle-dbg-current-step)))
      
      t))

(defun treadle-dbg-reset-circuit-low ()
   "Step the circuit"

   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   (tq-enqueue treadle-dbg-tq
      "randomize;reset 3\n"
      treadle-dbg-tq-regexp
      (list (current-buffer))
      #'(lambda (data str)
	   (with-current-buffer (first data)
	      (treadle-dbg-record-spurious-response-lines
		 str treadle-dbg-current-step)
	      (setq treadle-dbg-current-step 0)))
      
      t))

(defun treadle-dbg-show-components (command proc)
   "Retrieve something in the 'show' format.
Command should be a string like 'show state'.  PROC must take a
string argument."

   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   (tq-enqueue treadle-dbg-tq command treadle-dbg-tq-regexp
      (list (current-buffer) proc)
      #'(lambda (data str)
	   (with-current-buffer (first data)
	      (unless (eq treadle-dbg-current-buffer-type 'main)
	      	 (treadle-dbg-complain-bad-buffer))
	      (let* 
		 (  (proc (second data))
		    (begin-prompt-line
		       (string-match treadle-dbg-prompt-line-regexp-leading-cr
			  str))
		    (str1 (substring str 0 begin-prompt-line)))
		 (funcall proc str1))))
      t))



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

(defun treadle-dbg-redraw-widgets ()
   ""
   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   (widget-value-set
      treadle-dbg-widget-of-step-num
      treadle-dbg-current-step)
   (widget-value-set
      treadle-dbg-widget-of-freshness
      treadle-dbg-current-freshness)
   (widget-value-set
      treadle-dbg-widget-of-circuit-name
      treadle-dbg-current-circuit-name)

   (treadle-dbg-for-all-buttons
      #'(lambda (widget)
	   (let* 
	      ((widget (widget-get widget :node)))
	      (when (widget-get widget :value)
		 ;; This forces a redraw
		 (widget-value-set widget
		    (widget-value widget)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun treadle-dbg-read-new-boolean-val (prompt old-val)
   "Return it as number"

   (let*
      (  (default-string
	    ;; Reverse what was there, since there are only
	    ;; two possibilities
	    ;; bool number to string
	    (case (- 1 old-val)
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


(defun treadle-dbg-read-new-decimal-val (prompt old-val)
   ""
   ;; IMPROVE ME: Using type info, check new-val for bit width and
   ;; signedness.  Abort if new-val is not conformant.
   (let ((new-val
	    (read-number
	       prompt
	       old-val)))
      new-val))


(defun treadle-dbg-find-index-in-list (key str-list)
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
;; (treadle-dbg-find-index-in-list "b" '(("b")("a")))
;; (treadle-dbg-find-index-in-list "a" '(("b")("a")))

(defun treadle-dbg-read-new-enum-val (prompt fmt)
   ""

   (let*
      (  (key (second fmt))
	 (found (assoc key treadle-dbg-custom-enums)))
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
		  (treadle-dbg-find-index-in-list new-string strings)))
	    
	    (if new-val
	       new-val
	       (error "Lost the enum string %s" new-string)))
	 
	 (error "No such enum: %s" key))))

(defun treadle-dbg-read-new-val (prompt old-val perm-props)
   ""
   (let
      ((fmt perm-props))
      (case
	 (car fmt)
	 ;; Treat as a boolean
	 ((boolean)
	    (treadle-dbg-read-new-boolean-val prompt old-val))
	 ((enum)
	    (treadle-dbg-read-new-enum-val prompt fmt))
	 
	 (otherwise
	    (treadle-dbg-read-new-decimal-val prompt old-val)))))


(defun treadle-dbg-poke-value (sym new-val
				&optional extra-proc extra-data)
   "Poke NEW-VAL into the component named by SYM
Record the new value.  If EXTRA-PROC is non-nil, call it with extra-data."
   
   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))

   (let* 
      (  
	 (component (symbol-value sym))
	 (component-name (treadle-dbg-component-full-name component))
	 (current (treadle-dbg-component-current component))
	 (msg (concat "poke " component-name " "
		 (number-to-string new-val) "\n")))
      
      ;; IMPROVE ME:  Pre-filter inputs so we don't get errors here.
      (tq-enqueue treadle-dbg-tq
	 msg
	 treadle-dbg-tq-regexp
	 (list component new-val extra-proc extra-data)
	 #'(lambda (data str)
	      (let* 
		 ((had-problem
		     (treadle-dbg-parse-response-maybe-complain str))
		    (component (first data))
		    (new-val (second data))
		    (extra-proc (third data))
		    (extra-data (fourth data)))

		 (unless had-problem
		    ;; Set the component's value to that.
		    (setf (treadle-dbg-component-current component) new-val)
		    ;; IMPROVE ME: This could take a distinctive value
		    ;; so we can distinguish set-by-script from
		    ;; set-manually.  Could also use this to command
		    ;; "force" instead of "poke"
		    (setf (treadle-dbg-component-forced-p component) t)
		    (when extra-proc
		       (apply extra-proc extra-data)))))
	 t)))

(defun treadle-dbg-do-integer-edit&poke (widget widget-again &optional event)
   ""
   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   (let* 
      (  (sym (widget-get widget :value))
	 (component (symbol-value sym))
	 (component-name (treadle-dbg-component-full-name component))
	 (current (treadle-dbg-component-current component))
	 (perm-props
	    (treadle-dbg-get-perm-props (symbol-name sym)))
	 (new-val (treadle-dbg-read-new-val
		     (format "New value for %s: " component-name)
		     current
		     perm-props)))

      (when treadle-dbg-writing-script-p
	 (push
	    `(poke ,component-name ,new-val)
	    treadle-dbg-current-script-rv))
      (setq treadle-dbg-current-freshness "STALE")
      (widget-value-set
	 treadle-dbg-widget-of-freshness
	 "STALE")
      
      (treadle-dbg-poke-value
	 sym new-val
	 #'(lambda (widget widgets-buffer)
	      (with-current-buffer widgets-buffer
		 (widget-value-set widget (widget-value widget))))
	 (list widget (current-buffer)))))


;; ADAPT ME
'
(defun firrtl-dbg-run-script (script)
   "Run SCRIPT.
Script should be a list whose entries are in on of the forms:
 (poke component-name-string val)
 (step)"
   
   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))

   (widget-value-set
      treadle-dbg-widget-of-freshness
      "Running script")

   (dolist (line script)
      (case (first line)
	 (poke
	    (treadle-dbg-poke-value
	       (intern (second line) firrtl-dbg-obarray)
	       (third line)))
	 
	 (step
	    (treadle-dbg-step-circuit-low))))
   
   ;; All done, now reload and redisplay everything.
   (treadle-dbg-show-components
      "show state\n"
      #'treadle-dbg-record-state)
   (setq treadle-dbg-current-freshness "Script finished")

   (treadle-dbg-redraw-widgets))



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
   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   (setq treadle-dbg-current-script-rv '())
   (setq treadle-dbg-writing-script-p t))

;; ADAPT ME
'
(defun firrtl-dbg-stop-recording-script ()
   ""
   
   (interactive)
   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   (setq treadle-dbg-writing-script-p nil))

;; ADAPT ME
'
(defun firrtl-dbg-get-script ()
   ""
   
   (interactive)
   (unless (eq treadle-dbg-current-buffer-type 'main)
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
	 (legit-rx treadle-dbg-prompt-line-regexp)
	 (start-legit (string-match legit-rx str)))
      ;; Show any error that we get back
      (when (or (null start-legit) (> start-legit 0))
	 (message "%s" (substring str 0 start-legit))
	 t)))


;;;;;;;;;;;;;;;;;;;;

;; Example:
;; (treadle-dbg-call-until-done-w/timeout 4
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

(defun treadle-dbg-do-when-tq-empty (data proc)
   "PROC must take DATA as arguments."

   (tq-enqueue treadle-dbg-tq "\n" treadle-dbg-tq-regexp
      (cons proc data)
      #'(lambda (proc+data str)
	   (let* 
	      ((proc (first proc+data))
		 (data (cdr proc+data)))
	      (apply proc data)))
      t))

(defun treadle-dbg-initial-load (fir-file)
   ""
   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))

   (setq treadle-dbg-current-step 0)
   (setq treadle-dbg-current-freshness "FRESH")
   (setq treadle-dbg-current-circuit-name
      (treadle-dbg-get-circuit-name fir-file))

   (treadle-dbg-load-fir-file fir-file)

   (treadle-dbg-show-components
      "show state\n"
      #'treadle-dbg-record-state)
   (treadle-dbg-show-components
      "show inputs\n"
      #'treadle-dbg-record-inputs)
   (treadle-dbg-show-components
      "show outputs\n"
      #'treadle-dbg-record-outputs)

   (treadle-dbg-do-when-tq-empty
      (list (current-buffer))
      #'(lambda (buf)
	   (with-current-buffer buf
	      (unless (eq treadle-dbg-current-buffer-type 'main)
	      	 (treadle-dbg-complain-bad-buffer))

	      (setq
		 treadle-dbg-subname-tree
		 (treadle-dbg-sort-as-subname-tree treadle-dbg-subname-tree))

	      ;; Call symbol on every name
	      (mapatoms
		 #'treadle-dbg-get-symbol-data
		 treadle-dbg-obarray)

	      ;; Having enqueued the pre-requisites, now enqueue the
	      ;; widget drawing, so that the operations will be done
	      ;; in this order.
	      (treadle-dbg-do-when-tq-empty
		 (list (current-buffer))
		 #'(lambda (buf)
		      (with-current-buffer buf
			 (unless (eq treadle-dbg-current-buffer-type 'main)
			    (treadle-dbg-complain-bad-buffer))
			 ;; Draw the widgets and go.
			 (treadle-dbg-create-widgets))
		      (pop-to-buffer buf)))))))





(defun treadle-dbg-load-fir-file (fir-file)
   ""
   
   (let*
      ((command (concat "load " fir-file "\n")))
      (tq-enqueue treadle-dbg-tq command treadle-dbg-tq-regexp
	 nil nil t)))

(defun treadle-dbg-get-symbol-data (sym)
   ""
   (when sym
      (unless (string-blank-p (symbol-name sym))
	 (let*
	    ((command (concat "symbol ^" (symbol-name sym) "$\n")))
	    (message "Command = %s" command)
	    (tq-enqueue treadle-dbg-tq command treadle-dbg-tq-regexp
	       (list (current-buffer) (symbol-name sym))
	       #'(lambda (data str)
		    (with-current-buffer (first data)
		       (unless (eq treadle-dbg-current-buffer-type 'main)
			  (treadle-dbg-complain-bad-buffer))
		       ;;(message "Got %s" str)
		       (treadle-dbg-record-symbol-info
			  (second data)
			  str)))
	       t)))))




(define-derived-mode treadle-dbg-mode
   special-mode "Treadle-Dbg"
   "Major mode for Treadle debugger interface"
   :group 'treadle-dbg
   (progn
      (set-keymap-parent treadle-dbg-mode-map widget-keymap)
      (define-key treadle-dbg-mode-map  "\M-\r"
	 #'treadle-dbg-do-alt-interaction)))


(defun treadle-dbg-complain-bad-buffer (&optional msg)
   ""
   (error (or msg "This operation only makes sense in main buffer")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun treadle-dbg-compile-fir-file (fir-file-name)
   ""
   (when (stringp treadle-dbg-recompile-base-command)
      (let*
	 ((command
	     (concat
		treadle-dbg-recompile-base-command
		" "
		fir-file-name))
	    (buf
	       (generate-new-buffer
		  treadle-dbg-compile-process-buffer-name)))

	 (start-process
	    treadle-dbg-compile-process-name
	    buf
	    treadle-dbg-sbt-executable
	    command))))

(defun treadle-dbg-compile&restart ()
   ""
   (interactive)
   (unless (eq treadle-dbg-current-buffer-type 'main)
      (treadle-dbg-complain-bad-buffer))
   (if
      (not (stringp treadle-dbg-fir-file-location))
      (message "Need to set 'treadle-dbg-fir-file-location'"))
   (if (not (stringp treadle-dbg-recompile-base-command))
      (message "Need to set 'treadle-dbg-recompile-base-command'"))
   
   (when (stringp treadle-dbg-fir-file-location)
      (let
	 ((compile-process
	     (treadle-dbg-compile-fir-file treadle-dbg-fir-file-location)))
	 (message "Compiling...")
	 (when compile-process
	    (treadle-dbg-call-until-done-w/timeout
	       treadle-dbg-timeout
	       #'(lambda (main-buf fir-file compile-process)
		    (when (eq (process-status compile-process) 'exit)
		       (message "Restarting")
		       (let
			  ((wd (with-current-buffer main-buf
				  default-directory)))
			  (with-current-buffer main-buf
			     (treadle-dbg-shutdown))
			  (treadle-dbg wd fir-file))
		       t))

	       (list
		  (current-buffer)
		  treadle-dbg-fir-file-location
		  compile-process)
	       #'(lambda ()
		    (message "Compile process timed out"))
	       '())))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun treadle-dbg-compare-file-attributes-most-recent-mod (a b)
   ""

   
   (let* 
      ((a-time (seventh a))
	 (b-time (seventh b))
	 (done nil)
	 (result nil))
      (while (not done)
	 (let* 
	    ((a-1 (car-safe a-time))
	       (b-1 (car-safe b-time)))
	    (cond
	       ((or (not a-1) (not b-1))
		  ;; Ran out of data to compare
		  (setq done t))
	       ((eql a-1 b-1)
		  ;; The same, so use the next ones
		  (progn
		     (setq a-time (cdr-safe a-time))
		     (setq b-time (cdr-safe b-time))))
	       (t
		  (setq done t)
		  (setq result (> a-1 b-1))))))
      result))


(defun treadle-dbg-guess-best-fir-file (start-dir)
   ""
   
   (let*
      (  (test-run-dir
	    (file-name-as-directory
	       (concat
		  (file-name-as-directory start-dir)
		  "test_run_dir")))
	 ;; Exclude ".." and "."
	 (testrun-subdirs
	    ;; Regexp:  Exclude dot files.
	    (directory-files-and-attributes test-run-dir
	       t "^[^\\.]")))
      
      (unless (null testrun-subdirs)
	 (let* 
	    (
	       (best-dir
		  (car
		     (sort testrun-subdirs
			#'treadle-dbg-compare-file-attributes-most-recent-mod)))
	       (dir-name (car best-dir))
	       (dir (file-name-as-directory dir-name))
	       (list-in-dir
		  (directory-files dir t ".*\.fir$" t))
	       ;; IMPROVE ME:  Check unless null list-in-dir
	       (best-file-abs
		  (car list-in-dir)))
	    best-file-abs))))

(defun treadle-dbg-read-fir-file-name (wd)
   "Read or compute the fir file name, relative to WD"

   (let*
      ((best-file
	  (file-relative-name
	     (treadle-dbg-guess-best-fir-file wd)
	     wd)))
      (case treadle-dbg-how-to-find-fir-file
	 (always-use-latest-test
	    (let
	       ((best-file
		   (file-relative-name
		      (treadle-dbg-guess-best-fir-file wd)
		      wd)))
	       best-file))
	 (manual
	    (let
	       ((best-file
		   (file-relative-name
		      (treadle-dbg-guess-best-fir-file wd)
		      wd)))
	       (read-file-name
		  "FIRRTL file: "
		  wd best-file
		  'confirm nil)))
	 (use-custom-file
	    treadle-dbg-fir-file-location))))

(defun treadle-dbg-get-circuit-name (fir-file)
   ""

   (with-temp-buffer
      (insert-file-contents fir-file)
      (let* 
	 ((circuit-name nil))
	 (while (and (null circuit-name) (not (eobp)))
	    (if
	       (looking-at "circuit \\([a-zA-Z]+\\) *: *")
	       (setq circuit-name (match-string 1))
	       (forward-line 1)))
	 circuit-name)))

(defun treadle-dbg (working-directory fir-file)
   ""
   
   (interactive
      (let* 
	 ((wd
	     (let
		((file-name-history treadle-dbg-directory-history))
		(read-directory-name "Working directory: ")))
	    (ff (treadle-dbg-read-fir-file-name wd)))
	 (list wd ff)))
   

   (let*
      (
	 (buf-name "*TREADLE*")
	 (main-buf
	    (generate-new-buffer buf-name)))
      (with-current-buffer main-buf
	 (treadle-dbg-mode)
	 (setq default-directory working-directory)
	 ;; Set up most of the local variables.  Some are set further
	 ;; down as their objects are created.
	 (setq treadle-dbg-current-buffer-type 'main)

	 ;; Init the "easy" local variables
	 (setq treadle-dbg-obarray
	    (make-vector treadle-dbg-obarray-default-size nil))
	 (setq treadle-dbg-obarray-perm-props
	    (make-vector treadle-dbg-obarray-default-size nil))

	 (setq treadle-dbg-process-buffer
	    (generate-new-buffer treadle-dbg-process-buffer-name))

	 (with-current-buffer treadle-dbg-process-buffer
	    (setq default-directory working-directory)
	    (setq treadle-dbg-main-buffer main-buf))

	 (setq treadle-dbg-perm-props-buffer
	    (find-file-noselect (treadle-dbg-get-perm-props-filename)))
	 (with-current-buffer treadle-dbg-perm-props-buffer
	    (setq treadle-dbg-current-buffer-type 'perms-file)
	    ;; Arrange for save operations to first write our data
	    ;; into the buffer.
	    (add-hook 'write-contents-functions
	       'treadle-dbg-write-perms-to-buffer nil t)
	    (setq treadle-dbg-main-buffer main-buf)
	    ;; The buffer will be empty the first time we ever visit
	    ;; the file, so ensure that an object can be read.
	    (when (eql (buffer-size) 0)
	       (insert "nil\n")))
	 

	 ;; Read the old perms values
	 (treadle-dbg-load-all-perms
	    (with-current-buffer treadle-dbg-perm-props-buffer
	       (treadle-dbg-read-object-from-buffer)))

	 (setq treadle-dbg-process
	    (let ((default-directory working-directory))
	       (start-process
		  treadle-dbg-process-name
		  treadle-dbg-process-buffer
		  treadle-dbg-sbt-executable
		  ;; Quoting this string with shell-quote-argument
		  ;; actually messes us up.
		  treadle-dbg-repl-launch-command)))

	 ;;(hack-dir-local-variables-non-file-buffer)
	 ;;(treadle-dbg-copy-alist-to-perms)

	 (treadle-dbg-call-until-done-w/timeout
	    treadle-dbg-timeout
	    #'(lambda (process main-buf fir-file)
		 (when
		    (treadle-dbg-process-is-ready-p process)
		    (message "Debugger process is ready")
		    (let* 
		       ((tq (tq-create process)))
		       (with-current-buffer main-buf
			  (setq treadle-dbg-tq tq)
			  (treadle-dbg-initial-load fir-file)))
		    ;; Indicate that we have succeeded
		    t))
	    ;; Pass fir-file
	    (list treadle-dbg-process main-buf fir-file)
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
