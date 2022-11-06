(in-package #:compiler)


(defun write-push (segment index)
  "Writes a VM push command."
  (concatenate 'string "push " segment " " (write-to-string index)))

(defun write-pop (segment index)
  "Writes a VM pop command."
  (concatenate 'string "pop " segment " " (write-to-string index)))

(defun write-arithmetic (command)
  "Writes a VM arithmetic command."
  command)

(defun write-label (label)
  "Writes a VM label command."
  (concatenate 'string "label " label))

(defun write-goto (label)
  "Writes a VM goto command."
  (concatenate 'string "goto " label))

(defun write-if (label)
  "Writes a VM if-goto command."
  (concatenate 'string "if-goto " label))

(defun write-call (name nargs)
  "Writes a VM call command."
  (concatenate 'string "call " name " " (write-to-string nargs)))

(defun write-function (name nlocals)
  "Writes a VM function command."
  (concatenate 'string "function " name " " (write-to-string nlocals)))

(defun write-return ()
  "Writes a VM return command."
  "return")
