(defpackage :gendoc-docs
  (:use #:cl #:gendoc)
  (:export #:generate))

(in-package :gendoc-docs)

(defun generate ()
  (gendoc (:output-system :cl-gendoc-docs
           :output-filename "gendoc.html"
           :css "simple.css")
    (:mdf "intro.md")
    (:apiref :gendoc)))
