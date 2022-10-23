;;;; compiler.lisp

(in-package #:compiler)

(defun remove-comment (str)
  "Remove comment and trailing spaces."
  (let ((idx (search "//" str)))
    (if idx
	(string-trim '(#\space #\tab) (subseq str 0 idx))
	(string-trim '(#\space #\tab) str))))

(defun get-instructions (file)
  (let ((lines (uiop:read-file-lines file)))
    (mapcar #'remove-comment
	    (remove-if
	     #'(lambda (str) (or (string= "" (string-trim '(#\space #\tab) str))
				 (string= (char (string-trim '(#\space #\tab) str) 0) "/")
				 (and (> (length str) 1) (string= (char str 1) "*"))))
	     lines))))

(defun jack-analyzer (dir)
  (let* ((jack-files (directory (concatenate 'string dir "/*.jack")))
	 (output-folder (concatenate 'string dir "/analyzed/")))
    (dolist (jack-file jack-files)
      (let ((xml-file (concatenate 'string output-folder (pathname-name jack-file) ".xml")))
	(with-open-file (stream xml-file :direction :output :if-exists :supersede)
	  (jack-tokenizer jack-file stream))))))

(defun jack-tokenizer (jack-file stream)
  (format stream "<tokens>~%")
  (let ((lines (get-instructions jack-file)))
    (dolist (line lines)
      (let ((i 0)
	    (j 0))
	(loop for c across line
	      do (incf j)
	      when (and (not (string= #\" (char line i)))
			(or (char= #\space c) (member c *symbols* :test 'string=)))
		do (when (> j (1+ i)) (write-token (subseq line i (1- j)) stream))
		   (setf i j)
		   (when (not (char= #\space c)) (write-token c stream))
	      when (and (< i (1- j)) (string= #\" c) (string= #\" (char line i)))
		do (write-token (subseq line i j) stream)
		   (setf i j)))))
  (format stream "</tokens>~%"))

(defun tokenizer (jack-file)
  "Returns a list of tokens from jack-file."
  (let ((lines (get-instructions jack-file))
	(token-list '()))
    (dolist (line lines)
      (let ((i 0)
	    (j 0))
	(loop for c across line
	      do (incf j)
	      when (and (not (string= #\" (char line i)))
			(or (char= #\space c) (member c *symbols* :test 'string=)))
		do (when (> j (1+ i))
		     (setf token-list (append token-list (list (subseq line i (1- j))))))
		   (setf i j)
		   (when (not (char= #\space c))
		     (setf token-list (append token-list (list c))))
	      when (and (< i (1- j)) (string= #\" c) (string= #\" (char line i)))
		do (setf token-list (append token-list (list (subseq line i j))))
		   (setf i j))))
    token-list))

(defun write-token (token stream)
  (let ((type-of-token (token-type token)))
   (cond ((eql 'keyword type-of-token)
	  (format stream (concatenate 'string "<keyword> " (jack-keyword token) " </keyword>~%")))
	 ((eql 'symbol type-of-token)
	  (format stream (concatenate 'string "<symbol> " (jack-symbol token) " </symbol>~%")))
	 ((eql 'int-const type-of-token)
	  (format stream (concatenate 'string "<integerConstant> " (jack-intval token)
				      " </integerConstant>~%")))
	 ((eql 'string-const type-of-token)
	  (format stream (concatenate 'string "<stringConstant> " (jack-stringval token)
				      " </stringConstant>~%")))
	 ((eql 'identifier type-of-token)
	  (format stream (concatenate 'string "<identifier> " (jack-identifier token)
				      " </identifier>~%")))
	 (t (error "Token not recognized.")))))

(defparameter *keywords*
  '("class" "constructor" "function" "method" "field" "static" "var"
    "int" "char" "boolean" "void" "true" "false" "null" "this" "let"
    "do" "if" "else" "while" "return"))

(defparameter *symbols*
  '("{" "}" "(" ")" "[" "]" "." "," ";" "+" "-" "*" "/" "&" "|" "<" ">" "=" "~"))

(defun token-type (token)
  "Returns one of the five types: KEYWORD, SYMBOL, IDENTIFIER, INT-CONST and STRING-CONST."
  (cond ((member token *keywords* :test 'string=)
	 'keyword)
	((member token *symbols* :test 'string=)
	 'symbol)
	((every #'digit-char-p token)
	 'int-const)
	((and (char= #\" (char token 0)) (char= #\" (char token (1- (length token)))))
	 'string-const)
	((and (not (digit-char-p (char token 0)))
	      (every #'(lambda (x) (or (alphanumericp x) (string= "_" x))) token))
	 'identifier)
	(t (error "Token not recognized."))))

(defun jack-keyword (token)
  "Returns the keyword which is the current token."
  (if (member token *keywords* :test 'string=)
      token
      (error "Token is not a keyword.")))

(defun jack-symbol (token)
  "Returns the character which is the current token."
  (let ((token (string token)))
   (when (member token *symbols* :test 'string=)
     (cond ((string= #\< token) "&lt;")
	   ((string= #\> token) "&gt;")
	   ((string= #\& token) "&amp;")
	   ((string= #\~ token) "~~")
	   (t token)))))

(defun jack-identifier (token)
  "Returns the identifier which is the current token."
  (if (and (not (digit-char-p (char token 0)))
	   (every #'(lambda (x) (or (alphanumericp x) (string= "_" x))) token))
      token
      (error "Token is not an identifier.")))

(defun jack-intval (token)
  "Returns the integer string of the current token."
  (if (every #'digit-char-p token)
      token
      (error "Token is not an integer.")))

(defun jack-stringval (token)
  "Returns the string of the current token."
  (if (and (char= #\" (char token 0)) (char= #\" (char token (1- (length token)))))
      (subseq token 1 (1- (length token)))
      (error "Token is not a string.")))

(jack-analyzer "~/nand2tetris/projects/10/ArrayTest")
(jack-analyzer "~/nand2tetris/projects/10/Square")

;; (print (tokenizer "~/nand2tetris/projects/10/ArrayTest/Main.jack"))

