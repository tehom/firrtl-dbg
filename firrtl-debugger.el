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

;; Copied for convenience
;; (defun widget-tree-ancestor (widget)
;;   (let ((parent (car (get widget 'widget-type))))
;;     (if parent
;;         (cons widget (widget-tree-ancestor parent)))))
;; 
;; (defvar widget-tree-list nil
;;   "Inherit tree of all widget")
;; 
;; (defun widget-tree-build ()
;;   (let (list seen ancestor tree-list)
;;     (mapatoms (lambda (a)
;;                 (and (get a 'widget-type)
;;                      (push a list)))
;;               obarray)
;;     (setq list (remq 'default list))
;;     (while list
;;       (setq ancestor (nreverse (widget-tree-ancestor (car list))))
;;       (setq parent 'default)
;;       (dolist (type ancestor)
;;         (unless (member type seen)
;;           (setq alist (assoc parent tree-list)
;;                 tree-list (delq alist tree-list)
;;                 alist (cons parent (cons type (cdr alist)))
;;                 tree-list (cons alist tree-list)
;;                 list (delq type list))
;;           (push type seen))
;;         (setq parent type)))
;;     (setq widget-tree-list tree-list)))
;; 
;; (defun widget-tree-browse (but &rest ignore)
;;   (if (= (length (window-list)) 1)
;;       (split-window))
;;   (save-selected-window
;;     (other-window 1)
;;     (widget-browse (intern (widget-get but :tag)))))
;; 
;; (defun widget-tree-widget (type)
;;   (let ((list (assoc type widget-tree-list)))
;;     (if list
;;         `(tree-widget
;;           :node (push-button
;;                  :tag ,(symbol-name type)
;;                  :format "%[%t%]\n"
;;                  :notify widget-tree-browse)
;;           :dynargs widget-tree-expand)
;;       `(push-button
;;         :format "%[%t%]\n"
;;         :tag ,(symbol-name type)
;;         :notify widget-tree-browse))))
;; 
;; (defun widget-tree-expand (tree)
;;   (or (widget-get tree :args)
;;       (let ((type (intern (widget-get (tree-widget-node tree) :tag))))
;;         (mapcar 'widget-tree-widget
;;                 (cdr (assoc type widget-tree-list))))))
;; 
;; (defun widget-tree ()
;;   (widget-insert
;;    "This is a list of all defined widget. The tree-widget show the
;; inherit relationship of the widget.\n\n")
;; 
;;   (widget-insert "  You can click the button to browse the widget.\n\n")
;;   (unless widget-tree-list
;;     (widget-tree-build))
;;   (widget-apply-action
;;    (widget-create (widget-tree-widget 'default)))
;;   (if (require 'tree-mode nil t)
;;       (tree-minor-mode t)
;;     (widget-insert "\n\n")
;;     (widget-insert "   I recommend you use ")
;;     (widget-create 'url-link
;;                    :button-prefix ""
;;                    :button-suffix ""
;;                    :format "%[tree-mode%]"
;;                    :button-face 'info-xref
;;                    "http://www.emacswiki.org/cgi-bin/wiki/tree-mode.el")
;;     (widget-insert " to browse tree-widget.\n\n")))
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Partly adapted

;; (defun widget-tree-ancestor (widget)
;;   (let ((parent (car (get widget 'widget-type))))
;;     (if parent
;;         (cons widget (widget-tree-ancestor parent)))))

;; (defvar widget-tree-list nil
;;   "Inherit tree of all widget")

;; (defun widget-tree-build ()
;;   (let (list seen ancestor tree-list)
;;     (mapatoms (lambda (a)
;;                 (and (get a 'widget-type)
;;                      (push a list)))
;;               obarray)
;;     (setq list (remq 'default list))
;;     (while list
;;       (setq ancestor (nreverse (widget-tree-ancestor (car list))))
;;       (setq parent 'default)
;;       (dolist (type ancestor)
;;         (unless (member type seen)
;;           (setq alist (assoc parent tree-list)
;;                 tree-list (delq alist tree-list)
;;                 alist (cons parent (cons type (cdr alist)))
;;                 tree-list (cons alist tree-list)
;;                 list (delq type list))
;;           (push type seen))
;;         (setq parent type)))
;;     (setq widget-tree-list tree-list)))

;; (defun widget-tree-browse (but &rest ignore)
;;   (if (= (length (window-list)) 1)
;;       (split-window))
;;   (save-selected-window
;;     (other-window 1)
;;     (widget-browse (intern (widget-get but :tag)))))

;; (defun widget-tree-widget (type cell)
;;    (message (format "Cell %S" cell))
;;    (let ((list (assoc type widget-tree-list)))
;;       (if list
;; 	 `(tree-widget
;; 	     :node (push-button
;; 		      :value ,cell
;; 		      :oldtag ,(symbol-name type)
;; 		      :tag ,(car cell)
;; 		      :format "%[%t%]\n"
;; 		      :notify widget-tree-browse)
;; 	     :dynargs widget-tree-expand)
;; 	 `(push-button
;; 	     :value ,cell
;; 	     :oldtag ,(symbol-name type)
;; 	     :tag ,(car cell)
;; 	     :format "%[%t%]\n"
;; 	     :notify widget-tree-browse))))

;; (defun widget-tree-expand (tree)
;;   (or (widget-get tree :args)
;;      (let ((type (intern (widget-get (tree-widget-node tree) :oldtag)))
;; 	     (alist (cddr (widget-get (tree-widget-node tree) :value))))
;; 	(message (format "Alist %S" alist)) ;; mapcar
;; 	;; Was "mapcar".  Limited by the shorter of the two lists
;;         (map 'list 'widget-tree-widget
;; 	   (cdr (assoc type widget-tree-list))
;; 	   alist))))

;; (defun widget-tree ()
;;   (widget-insert
;;    "This is a list of all defined widget. The tree-widget show the
;; inherit relationship of the widget.\n\n")

;;   (widget-insert "  You can click the button to browse the widget.\n\n")
;;   (unless widget-tree-list
;;     (widget-tree-build))
;;   (widget-apply-action
;;      (widget-create (widget-tree-widget 'default
;; 		       (cons "nono" firrtl-current-components))))
;;   (if (require 'tree-mode nil t)
;;       (tree-minor-mode t)
;;     (widget-insert "\n\n")
;;     (widget-insert "   I recommend you use ")
;;     (widget-create 'url-link
;;                    :button-prefix ""
;;                    :button-suffix ""
;;                    :format "%[tree-mode%]"
;;                    :button-face 'info-xref
;;                    "http://www.emacswiki.org/cgi-bin/wiki/tree-mode.el")
;;     (widget-insert " to browse tree-widget.\n\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KINDA WORKING

;; (defun widget-tree-widget (cell)
;;    (message (format "Cell %S" cell))
;;    (let ()
;;       (if (eq (second cell) 'list)
;; 	 `(tree-widget
;; 	     :node (push-button
;; 		      :value ,cell
;; 		      :tag ,(car cell)
;; 		      :format "%[%t%]\n"
;; 		      :notify widget-tree-browse)
;; 	     :dynargs widget-tree-expand)
;; 	 `(const
;; 	     :format "%v\n"
;; 	     :value ,(cdr cell)
;; 	     :value-create ,#'firrtl-dbg-insert-normal-component))))


;; (defun widget-tree-expand (tree)
;;   (or (widget-get tree :args)
;;      (let (
;; 	     (alist (cddr (widget-get (tree-widget-node tree) :value))))
;; 	(message (format "Alist %S" alist)) ;; mapcar
;; 	;; Was "mapcar".  Limited by the shorter of the two lists
;;         (map 'list 'widget-tree-widget
;; 	   alist))))

;; (defun widget-tree ()
;;   (widget-insert
;;    "This is a list of all defined widget. The tree-widget show the
;; inherit relationship of the widget.\n\n")

;;   (widget-insert "  You can click the button to browse the widget.\n\n")
;;   (widget-apply-action
;;      (widget-create (widget-tree-widget
;; 		       (cons "root" firrtl-current-components))))
;;   (if (require 'tree-mode nil t)
;;       (tree-minor-mode t)
;;     (widget-insert "\n\n")
;;     (widget-insert "   I recommend you use ")
;;     (widget-create 'url-link
;;                    :button-prefix ""
;;                    :button-suffix ""
;;                    :format "%[tree-mode%]"
;;                    :button-face 'info-xref
;;                    "http://www.emacswiki.org/cgi-bin/wiki/tree-mode.el")
;;     (widget-insert " to browse tree-widget.\n\n")))



;;;_ , Requires

(require 'cl)
(require 'parse-time) ;; For parse-integer
(require 'subr-x)
(require 'tree-widget)
;;;_. Body

;; Maybe valid-p can take a n/a value as well
(defstruct (component-value (:type list))
   ""
   v
   valid-p)

;; Add (:include firrtl-component) on each one after here.
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



;; Not a good approach.  Widgets hold all of this data, and outside
;; there we might as well spread it into variables.
'
(defstruct circuit-state
   "A state of the circuit"
   step
   freshness-str
   ;; Registers & memory live here too?
   )

;; Local variables
;; MAKE ME LOCAL in the mode

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

;; OBSOLETE
(defstruct (firrtl-subtree-info (:type list))
   ""
   was-found ;; Bool
   old-tag+tree
   )

(defun firrtl-write-to-component (tree subname-list proc data)
   "
TREE should be '(list subtree...).  We don't try to handle the case where TREE begins with something else such as a structure tag.
PROC should both take and return an individual element"
   (let* 
      (  (tree (or tree '(list)))
	 (subtree-info-list '())
	 ;; A list* of tag, 'list, and current alist being explored
	 (current-tag+tree (cons nil tree)))

      ;; Drill down to the component that we want.
      (dolist (subname subname-list)
	 ;; This assumes that what we're exploring is an alist and
	 ;; doesn't try to handle structure tags.
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
		  (setq current-tag+tree (cons subname '(list)))))))
      
      ;; Now current-tag+tree points at the leaf that corresponds to
      ;; subname-list

      ;; Alter the component.  This may erase a 'list tag
      (setcdr current-tag+tree
	 (funcall proc (cdr current-tag+tree) data))

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
   #'(lambda (old data)
	(cons data nil))
   'my-data)

'
(firrtl-write-to-component '(list ("a" list ("b" my-data))) '("a" "b")
   #'(lambda (old data)
	(cons data nil))
    'new-data)

'
(firrtl-write-to-component '(list ("a" list ("b" my-data))) '("a" "c")
   #'(lambda (old data)
	(cons data nil))
    'new-data)

'
(firrtl-write-to-component '(list ("a" list ("b" my-data))) '("d" "b")
   #'(lambda (old data)
	(cons data nil))
    'new-data)


(defun firrtl-split-component-name (str)
   ""
   (split-string str "[._]+"))
'
(firrtl-split-component-name "io_a.b")

;; Helper to set up current data to develop on
(defun firrtl-write-to-current-components (comp)
   ""
   (setq
      firrtl-current-components
      (firrtl-write-to-component firrtl-current-components
	 (firrtl-split-component-name
	    (firrtl-component-full-name comp))
	 ;; No point keeping the old one.
	 #'(lambda (v data)
	      data)
	 comp)))

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
      (step (parse-integer (match-string 1 str)))
      ;; We need better info on this.  Only "FRESH" or "STALE"?
      (freshness-str (match-string 2 str)))
   

   ;;(make-circuit-state :step step :freshness-str freshness-str)
   )

(setq split
   (let* 
   ((input-str (second spl))
      ;; "Inputs: *"
      ;; "Outputs: *"
      ;; "Registers *: *"
      ;; "FutureRegisters: *"
      ;; "Ephemera: *"
      ;; "Memories" ;; This one may be different.  None written yet.
      (m (string-match "Inputs: *" input-str))
      (end (match-end 0))
      (input-str (substring input-str end))
      (split (split-string input-str ","))
      )

   ;; Use the list
   (first split)
   split))

;; "clock= 0"
'' (setq an-input " io_cellState=☠ 1☠")


(defun firrtl-dbg-parse-component (component-str)
   "Return type here is not settled"
   (let* 
      (
	 (m (string-match
	       "^ *\\([^=]+\\)=\\(☠?\\) *\\([0-9]+\\)\\(☠?\\)"
	       component-str))
	 (valid-p (string-empty-p (match-string 2 component-str)))
	 (value (parse-integer (match-string 3 component-str)))
	 )
      ;; 2 and 4 should match

      ;; First try to find the component, then make it if not found.

      ;; So let's return (name . component-value), then search component,
      ;; then always return a component.  We must split up name before
      ;; any of this happens.

      (make-component
	 :name (match-string 1 component-str)
	 :current
	 (make-component-value
	    :v value
	    :valid-p valid-p)
	 :next
	 (make-component-value
	    :v 0
	    :valid-p nil))))

(setq comp-list
   (mapcar #'firrtl-dbg-parse-component split))


(setq comp1
(let* 
   ((component-str " io_cellState=☠ 1☠")
      ;; " io_cellState=☠ 1☠"
      ;; "clock= 0"
      (m (string-match
	    "^ *\\([^=]+\\)=\\(☠?\\) *\\([0-9]+\\)\\(☠?\\)"
	    component-str))
      (valid-p (string-empty-p (match-string 2 component-str)))
      (value (parse-integer (match-string 3 component-str)))
      )
   ;; 2 and 4 should match

   ;; First try to find the component, then make it if not found.

   ;; So let's return (name . component-value), then search component,
   ;; then always return a component.  We must split up name before
   ;; any of this happens.

   (make-component
      :name (match-string 1 component-str)
      :current
      (make-component-value
	 :v value
	 :valid-p valid-p)
      :next
      (make-component-value
	 :v 0
	 :valid-p nil))))

;; https://www.emacswiki.org/emacs/widget-demo.el

;;(define-widget 'component-widget 'default
;;   "Widget to display a component")

;; For most components, non-editable.  Just displays it.

;; For input edit, use 'integer.  Also shows name etc.  Must figure
;; out whether it has been changed.  But widgets let you add a
;; callback, so just tell a registry that it has changed (Maybe also
;; have a face for "You set this")
;;

;; tree-mode seems very helpful for this.  :node will be the type of
;; the leaf widgets (a choice)

;; Extras: 

;; Add buttons and shortcuts for the functionality (step&show, multistep, reset)
;; Just if it's input, insert a value modification widget.
;; Allow hex or bin values, and arbitrary user value conversions (enums)
;; Darker face if it's invalid (:value-face)
;; Rearrange the buffer - push some to the top?
;; Add something to save a configuration (currently open/closed/value-format)
;; Find an existing one (markers and a hash table to them?  Search the top widget?)
;; Another buffer and structure defining enum value conversions?

;; (widget-insert "\n")
;; (widget-insert (component-name comp1))
;; (widget-insert "  ")
;; (widget-insert (int-to-string (component-value-v (component-current comp1))))

;; Displays a name, but our type is wrong
'
(widget-create 'push-button
   :format "%[%t%]\n"
   :tag (component-name comp1))
'
(widget-create 'const
   :format "%t  "
   :tag (component-name comp1))

'
(widget-create 'const
   :format "%v  "
   :value (component-name comp1))
'
(widget-create 'const
   :format "%v\n"
   :value (int-to-string (component-value-v (component-current comp1))))

;; How to make a group of these?  Make the individuals and group them.
'
(apply 'widget-convert 'const 
   (list
      :format "%v  "
      :value (component-name comp1)))

'
(apply 'widget-convert 'const 
   (list
      :format "%v\n"
      :value comp1
      :value-create 
      #'(lambda ()
	   (widget-insert "Whee!\n"))))

;; PLACEHOLDER
(defun firrtl-punt-notify (but &rest ignore)
   ""
   
   (let*
      ()
      
      ))

;; WORKS after tree-widget is loaded, but gets stuck on a copier.
;; What we want for the root.  firrtl-dbg-tree-browse will expand this.
'
(apply 'widget-convert
   `(tree-widget
       :node (push-button
		:data ,firrtl-current-components
		:tag "root"
		:format "%[%t%]\n"
		:notify firrtl-punt-notify)
       :dynargs firrtl-dbg-tree-expand))


;; There's also "register" and "io.input"
(defun firrtl-dbg-insert-normal-component (wid)
   "Insert an ephemeral component"
   (let* 
      ((v (widget-get wid :value)))
      ;; (message (format "Inserter %S" v))

      ;; 
      (widget-insert (firrtl-ephemeral-name v))
      (widget-insert "  ")
      (widget-insert
	 (int-to-string (component-value-v (firrtl-ephemeral-current v))))))




' 
(apply 'widget-create 'const 
   (list
      :format "%v\n"
      :value comp1
      :value-create #'firrtl-dbg-insert-normal-component))

;; Inserts widget into the buffer
;; (widget-apply widget :create)

;; NO
;; (defun firrtl-dbg-tree-ancestor (widget)
;;   (let ((parent (car (get widget 'widget-type))))
;;     (if parent
;;         (cons widget (firrtl-dbg-tree-ancestor parent)))))

;; ;; NO
;; (defvar firrtl-dbg-tree-list nil
;;   "Inherit tree of all widget")



;; ;; The root will be stored buffer-locally.  The respective nodes will
;; ;; be stored as tree's inner nodes' data.
;; (defun firrtl-dbg-tree-build ()
;;    ;; Make a list of the things
;;    '
;;   (let (list seen ancestor tree-list)
;;     (mapatoms (lambda (a)
;;                 (and (get a 'widget-type)
;;                      (push a list)))
;;               obarray)
;;     (setq list (remq 'default list))
;;     (while list
;;       (setq ancestor (nreverse (firrtl-dbg-tree-ancestor (car list))))
;;       (setq parent 'default)
;;       (dolist (type ancestor)
;;         (unless (member type seen)
;;           (setq alist (assoc parent tree-list)
;;                 tree-list (delq alist tree-list)
;;                 alist (cons parent (cons type (cdr alist)))
;;                 tree-list (cons alist tree-list)
;;                 list (delq type list))
;;           (push type seen))
;;         (setq parent type)))
;;      (setq firrtl-dbg-tree-list tree-list))
;;    ;; WRITE ME as proof of concept:

;;    )

;; (defun firrtl-dbg-tree-browse (but &rest ignore)
;;   (if (= (length (window-list)) 1)
;;       (split-window))
;;   (save-selected-window
;;     (other-window 1)
;;     (widget-browse (intern (widget-get but :tag)))))


;; Try applying this on comp-list.  But we must get it by name or name
;; component.
;; '
;; (defun firrtl-dbg-tree-widget (type)
;;    ;; firrtl-current-components
;;    ;; (assoc name firrtl-dbg-tree-list)  WRONG, that's the root.
;;    ;; So assoc it on a local copy?  Or find the whole name in the root?
;;   (let ((list (assoc type firrtl-dbg-tree-list)))
;;      (if list
;; 	;; Unchanged
;;         `(tree-widget
;; 	    :node (push-button
;; 		     ;; Add data: Data of the tree, including 'list
;; 		     :tag ,(symbol-name type) ;; Should be a prefix or similar.
;; 		     :format "%[%t%]\n"
;; 		     :notify firrtl-dbg-tree-browse)
;; 	    :dynargs firrtl-dbg-tree-expand)
;;        (const
;; 	  :format "%v\n"
;; 	  :value ,type
;; 	  :value-create #'firrtl-dbg-insert-normal-component))))
;; '
;; (defun firrtl-dbg-tree-expand (tree)
;;    (or (widget-get tree :args)
;;       ;; Earlier, store that ply of firrtl-current-components.
;;       (let (
;; 	      (alist (cdr (widget-get (tree-widget-node tree) :data)))
;; 	      (type (intern (widget-get (tree-widget-node tree) :tag))))
;; 	 ;;(insert data) ;; DEbug
;; 	 (mapcar #'firrtl-dbg-tree-widget alist)))) 


(defun firrtl-dbg-tree-widget (cell)
   ;;(message (format "Cell %S" cell))
   (let ()
      (if (eq (second cell) 'list)
	 `(tree-widget
	     :node (push-button
		      :value ,cell
		      :tag ,(car cell)
		      :format "%[%t%]\n"
		      ;; Notify something else, actually.
		      :notify widget-tree-browse)
	     :dynargs firrtl-dbg-tree-expand)
	 ;; This should depend on component type
	 `(const
	     :format "%v\n"
	     :value ,(cdr cell)
	     :value-create ,#'firrtl-dbg-insert-normal-component))))


(defun firrtl-dbg-tree-expand (tree)
   (or (widget-get tree :args)
      (let (
	      (alist (cddr (widget-get (tree-widget-node tree) :value))))
	 ;;(message (format "Alist %S" alist))
	 ;; Was "mapcar".
	 (map 'list 'firrtl-dbg-tree-widget
	    alist))))

(defun firrtl-dbg-tree ()
   (widget-insert "Preliminary:  FIRRTL debugger interface\n\n")
   (widget-apply-action
      (widget-create (firrtl-dbg-tree-widget
			(cons "root" firrtl-current-components))))
   (if (require 'tree-mode nil t)
      (tree-minor-mode t)
      (widget-insert "\n\n")))



;;;_. Footers
;;;_ , Provides

(provide 'firrtl-debugger)

;;;_ * Local emacs vars.
;;;_  + Local variables:
;;;_  + mode: allout
;;;_  + End:

;;;_ , End
;;; firrtl-debugger.el ends here
