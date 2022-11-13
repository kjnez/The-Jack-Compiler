;;;; compiler.asd

(asdf:defsystem #:compiler
  :description "The Jack Compiler"
  :author "Yingnan Cui"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:uiop #:split-sequence)
  :components ((:file "package")
               (:file "tokenizer")
	       (:file "symbol-table")
	       (:file "parser")
	       (:file "vm-writer")
	       (:file "compilation-engine")))
