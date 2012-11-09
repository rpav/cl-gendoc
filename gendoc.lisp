(in-package :gendoc)

 ;; ASDF

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-gendoc-load-op (system-name package-name function-symbol)
    "Define `PERFORM` and `OPERATION-DONE-P` for `ASDF:LOAD-OP` for the system
`SYSTEM-NAME`, causing `LOAD-OP` to always call `FUNCTION-SYMBOL` in the
package `PACKAGE-NAME`.

For this to be useful, the specified function should call `GENDOC` to actually
generate documentation."
    `(progn
       (defmethod asdf:perform :after ((o asdf:load-op) (c (eql (asdf:find-system ,system-name))))
         (let ((fn (find-symbol (symbol-name ,function-symbol) (find-package ,package-name))))
           (funcall fn)))

       (defmethod asdf:operation-done-p ((o asdf:load-op) (c (eql (asdf:find-system ,system-name))))
         nil))))

 ;; Parts

(defvar *part-processor* (make-hash-table))

(defun add-processor (name function)
  "Call `FUNCTION` when the component named `NAME` is encountered in the
gendoc spec."
  (setf (gethash name *part-processor*) function))

 ;; Utility

(defun read-all (file-stream)
  (let ((output (make-array (file-length file-stream)
                            :element-type (stream-element-type file-stream))))
    (read-sequence output file-stream)
    output))

#+sbcl
(defun arglist (function)
  (sb-introspect:function-lambda-list function))

#+ccl
(defun arglist (function)
  (multiple-value-bind (arglist binding)
      (ccl:arglist function)
    (declare (ignore binding))
    arglist))

#+clisp
(defun arglist (function)
  (ext:arglist function))

#-(or sbcl ccl clisp)
(defun arglist (function)
  (warn "Your implementation doesn't currently support ARGLIST.  Submit a patch!")
  "...")

 ;; Gendoc Macro

(defmacro gendoc ((&key (stream '*standard-output*) (title "GenDoc Documentation") css path
                   output-system output-component output-filename)
                  &body parts)
  "=> string or no values

Generate documentation based on `PARTS`.  Each part is processed in order
and the output is written to `STREAM`, or, if specified, `OUTPUT-FILE` is
overwritten (`:if-exists :supersede`).

`TITLE` and `CSS` specify their HTML counterparts.

`OUTPUT-SYSTEM` and `OUTPUT-COMPONENT` may be specified; if so, they are
consulted to set `*DEFAULT-PATHNAME-DEFAULTS*`, and files are read from
and written to this location.

`PATH` may be specified instead or along with `OUTPUT-SYSTEM` and
`OUTPUT-COMPONENT`; if both are specified, `PATH` overrides."
  (let ((part (gensym))
        (proc-name (gensym))
        (processor (gensym))
        (html (gensym))
        (actual-filename (gensym))
        (actual-stream (gensym))
        (close-stream-p (gensym)))
    `(let* ((*default-pathname-defaults*
              (or ,path
                  (when ,output-system
                    (asdf:component-pathname (asdf:find-component ,output-system ,output-component)))
                  *default-pathname-defaults*))
            (3bmd-code-blocks:*code-blocks* t)
            (,actual-filename (let ((output-filename ,output-filename))
                                (and output-filename (merge-pathnames output-filename))))
            (,actual-stream ,stream)
            (,close-stream-p))
       (unwind-protect
            (progn
              (when ,actual-filename
                (setf ,actual-stream (open ,actual-filename :direction :output :if-exists :supersede))
                (setf ,close-stream-p t))
              (cl-who:with-html-output (,html ,actual-stream :prologue t :indent t)
                (:head (and ,title (cl-who:htm (:title (cl-who:str ,title))))
                       (and ,css (cl-who:htm (:link :rel "stylesheet" :type "text/css" :href ,css))))
                (:html
                 (loop for ,part in ',parts
                       do (let* ((,proc-name (pop ,part))
                                 (,processor (gethash ,proc-name *part-processor*)))
                            (funcall ,processor ,actual-stream ,proc-name ,part))))))
         (when ,close-stream-p
           (close ,actual-stream)))
       (values))))

 ;; Processors

(defun process-text-file (stream name part)
  (declare (ignore name))
  (let ((filename (car part))
        (*standard-output* stream))
    (write-line "<pre>")
    (with-open-file (input filename)
      (princ (read-all input)))
    (write-line "</pre>")))

(add-processor :text-file #'process-text-file)
(add-processor :txt #'process-text-file)

(defun process-markdown-file (stream name part)
  (declare (ignore name))
  (let ((filename (car part)))
    (with-open-file (input filename)
      (3bmd:parse-string-and-print-to-stream (read-all input) stream))))

(add-processor :markdown-file #'process-markdown-file)
(add-processor :mdf #'process-markdown-file)

(defun special-p (symbol)
  (or (boundp symbol)
      (documentation symbol 'variable)))

(defun function-p (symbol)
  (and (fboundp symbol)
       (not (macro-function symbol))))

(defun macro-p (symbol)
  (macro-function symbol))

(defun apiref-symbols (type package)
  (let (symbols)
    (loop for symbol being each external-symbol in package
          if (ecase type
               (:special (special-p symbol))
               (:macro (macro-p symbol))
               (:function (function-p symbol)))
            do (push symbol symbols))
    (sort symbols #'string<)))

(defun apiref-spec (type sym)
  (declare (ignore type))
  (string sym))

(defun apiref-lambda (type sym)
  (ecase type
    (:special "")
    ((or :macro :function)
     (if (arglist sym)
         (write-to-string (arglist sym))
         "()"))))

(defun apiref-result (type sym)
  (ecase type
    (:special "")
    ((or :macro :function)
     (let ((ds (documentation sym 'function)))
       (if (and (> (length ds) 0)
                (string= (subseq ds 0 2) "=>"))
           (subseq ds 0 (position #\Newline ds))
           "")))))

(defun apiref-doc (type sym)
  (or 
   (ecase type
     (:special (documentation sym 'variable))
     ((or :macro :function)
      (let ((ds (documentation sym 'function)))
        (if (and (> (length ds) 0)
                 (string= (subseq ds 0 2) "=>"))
            (let ((br (position #\Newline ds)))
	      (if br (subseq ds br) "")) 
            ds))))
   "*Undocumented!*"))

(defun apiref-section-symbol (stream type symbol)
  (cl-who:with-html-output (html stream :indent t)
    (:a :name symbol :class "apiref-row")
    (:div :class "apiref-spec"
      (cl-who:str (apiref-spec type symbol)))
    (:div :class "apiref-lambda"
      (cl-who:str (apiref-lambda type symbol)))
    (:div :class "apiref-result"
      (cl-who:str (apiref-result type symbol)))
    (:div :class "apiref-doc"
      (cl-who:str
       (with-output-to-string (s)
         (3bmd:parse-string-and-print-to-stream (apiref-doc type symbol) s))))))

(defun gen-apiref (stream package)
  (let ((*package* package)
        (specials (apiref-symbols :special package))
        (functions (apiref-symbols :function package))
        (macros (apiref-symbols :macro package)))
    (cl-who:with-html-output (html stream :indent t)
      (when specials
        (cl-who:htm (:h2 "Special Variables"))
        (loop for sym in specials
              do (apiref-section-symbol stream :special sym)))
      (when functions
        (cl-who:htm (:h2 "Functions"))
        (loop for sym in functions
              do (apiref-section-symbol stream :function sym)))
      (when macros
        (cl-who:htm (:h2 "Macros"))
        (loop for sym in macros
              do (apiref-section-symbol stream :macro sym))))))

(defun process-apiref (stream name package-list)
  (declare (ignore name))
  (cl-who:with-html-output (html stream :indent t)
    (loop for package-name in package-list
          do (cl-who:htm
              (:a :name (concatenate 'string
                                     "REFERENCE-"
                                     (string package-name)))
              (:h1 "Reference: " (cl-who:str package-name))
              (let ((package (find-package package-name)))
                (if package
                    (gen-apiref stream package)
                    (cl-who:htm (:p "Package not found."))))))))

(add-processor :apiref 'process-apiref)
