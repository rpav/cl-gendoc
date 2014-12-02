(defsystem :cl-gendoc
  :description "cl-gendoc: Simple component-based documentation generator"
  :version "1.0"
  :author "Ryan Pavlik"
  :license "LLGPL, BSD"

  :depends-on (#+sbcl :sb-introspect
               :cl-who :3bmd :3bmd-ext-code-blocks)
  :serial t

  :components
  ((:file "package")
   (:file "gendoc")))

(defsystem :cl-gendoc-docs
  :depends-on (:cl-gendoc)

  :pathname "doc"
  :serial t
  :components
  ((:file "gendoc-docs")
   (:static-file "intro.md")))

;; (define-gendoc-load-op :cl-gendoc-docs :gendoc-docs 'generate) =>
(defmethod perform :after ((o load-op) (c (eql (find-system :cl-gendoc-docs))))
  (let ((fn (find-symbol (symbol-name 'generate) (find-package :gendoc-docs))))
    (funcall fn)))

(defmethod operation-done-p ((o load-op) (c (eql (find-system :cl-gendoc-docs))))
  nil)
