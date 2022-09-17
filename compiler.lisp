;;;; compiler.lisp

(in-package #:compiler)

(defun remove-comment (str)
  "Remove comment and trailing spaces."
  (let ((idx (search "//" str)))
    (if idx
	(string-trim " " (subseq str 0 idx))
	(string-trim " " str))))

(defun get-instructions (file)
  (let ((lines (uiop:read-file-lines file)))
    (mapcar #'remove-comment
	    (remove-if
	     #'(lambda (str) (or (string= "" str) (string= (elt str 0) "/")))
	     lines))))

(defun jack-analyzer (dir)
  (let* ((jack-files (directory (concatenate 'string dir "/*.jack")))
	 (output-folder (concatenate 'string dir "/analyzed/")))
    (dolist (jack-file jack-files)
      (let ((xml-file (concatenate 'string output-folder (pathname-name jack-file) ".xml")))
	(with-open-file (stream xml-file :direction :output :if-exists :supersede)
	  (jack-tokenizer jack-file stream))))))

(defun jack-tokenizer (jack-file stream))
