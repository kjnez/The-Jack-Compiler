(in-package #:compiler)


(defun write-push (segment index)
  "Writes a VM push command."
  (if segment
      (concatenate 'string "push " segment " " (write-to-string index))
      (concatenate 'string "push " (write-to-string index))))

(defun write-pop (segment index)
  "Writes a VM pop command."
  (concatenate 'string "pop " segment " " (write-to-string index)))

(defun write-arithmetic (command)
  "Writes a VM arithmetic command."
  (cond ((string= #\+ command)
	 "add")
	((string= #\- command)
	 "sub")
	((string= #\= command)
	 "eq")
	((string= #\> command)
	 "gt")
	((string= #\< command)
	 "lt")
	((string= #\& command)
	 "and")
	((string= #\| command)
	 "or")
	((string= #\* command)
	 (write-call "Math.multiply" 2))
	((string= #\/ command)
	 (write-call "Math.divide" 2))
	((or (string= #\~ command) (string= "not" command))
	 "not")
	((string= "neg" command)
	 "neg")
	(t (error "Arithmetic command not recognized."))))

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
