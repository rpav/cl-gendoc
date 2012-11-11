# cl-gendoc

This is a simple but flexible modular document generator for Common
Lisp, because I couldn't find something similar:

```lisp
(gendoc (:output-filename "docs.html"
         :css "simple.css")
  (:mdf "intro.md")
  (:mdf "details.md")
  (:apiref :some-package :another-package)
  (:mdf "closing.md"))
```

Of some interest is probably the API reference generator:

```lisp
(defun some-function (X)
  "=> output-forms

This takes `X` and produces *output-forms*"
  ...)
```

The docstring is processed as markdown with `3bmd`, optionally
specifying a return-spec if the first line starts with `"=>"`.

## Generating Documentation

Documentation isn't *just* about having a reference, but it's nice to
integrate docstrings and separately-written material.  Gendoc does
this by processing all the parts into HTML and concatenating them into
a single file (or stream).

Currently the following component types are understood:

* :text-file, :txt - Add the text file contents inside a &lt;pre&gt;
  tag.

* :markdown-file, :mdf - Process the file with 3bmd (with code-blocks
  enabled).

* :apiref - Generate an API reference for the specified packages. This
  is fairly rudimentary at this point, as it only includes special
  variables, functions, and macros.  Adding more is fairly trivial
  however and I am open to suggestions.

Clearly there could be more; TOC generation would be neat, HTML would
be trivial.  Patches welcome.

## Adding Component Types

New components types are trivial to add:

```lisp
(defun my-processor (stream name args)
  ...)

(gendoc:add-processor :my-processor 'my-processor)

(gendoc (...)
  (:my-processor x y z))
```

`NAME` is the name the processor was called with (in this example,
`:my-processor`), and `ARGS` is the `cdr` of the part specified (`(x y z)`).

You should output HTML to `STREAM`.

## ASDF

In your ASD file specify:

```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system :gendoc))

  :

(defsystem :my-system-docs
  :pathname PATHNAME-TO-DOCUMENTATION
  ...)

(gendoc:define-gendoc-load-op :my-system-docs :my-docs-package
                              'generate-function)
```

When you `(asdf:load :my-system-docs)`, it will call
`GENERATE-FUNCTION`, which presumably calls `GENDOC` to generate
documentation.

`GENDOC` can use the pathname of the system to look for the rest of
your files, and place output there.

**Note:** This also defines `OPERATION-DONE-P` to `NIL` for the system
you specify.  You probably don't want to do this for the main system,
or it will be reloaded everytime ASDF looks for it.

## Anchors

The following `id` attributes are generated and can be used as anchors:

* Input files (text and markdown) get the id `file-<filename>-<file-ending>`, so `README.md` becomes `file-REAMDE-md`.
  [(Try it)](#file-intro-md)

* The reference section for a package gets the id `reference-<package>`, e.g. the package `:GENDOC` results in the id `reference-gendoc`.
  [(Try it)](#reference-gendoc)

* The specials/functions/macros section get the id `<package>-specials`, `<package>-functions` and `<package>-macros`.
  [(Try it)](#gendoc-macros)

* Each entry for a symbol gets the id `apiref-<symbol>`.
  [(Try it)](#apiref-gendoc)

**Note:** All ids (except for input files) are lowercase.

## CSS

The following classes are defined in the resulting html and can be used with CSS:

* Text files are put into `<article>` tags with the class `text-article`.

* Markdown files are put into `<article>` tags with the class `markdown-article`.

* The reference for each package is an `<article>` with the class `apiref-article`.

* Each specials/functions/macros block is a `<section>` with the class `section-<specials|functions|macros>`.

* Each entry in the symbol list of a package is a `<section>` with the class `section-apiref-item`.

* Each symbol in the symbol list is enclosed in a `<div>` with the class `apiref-spec`.

* Each lambda list is enclosed in a `<div>` with the class `apiref-lambda`.

* The result line ("=> ...") of a docstring lives in a `<div>` with the class `apiref-result`.

* The rest of the docstring becomes a `<div>` with the class `apiref-doc`.

Tip: to convert your symbols and lambda lists to lowercase, you can use the CSS property `text-transform: lowercase;`