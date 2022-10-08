(in-package #:compiler)

(defun compile-class (token-list)
  "The highest level parser for each class. Returns a new token-list and a parsed-list.
   Structure: 'class' className '{' classVarDec* subroutineDec* '}'"
  (let ((parsed-list '()))
    (if (string= "class" (first token-list))
	(setf parsed-list (list "<class>" "<keyword> class </keyword>"))
	(error "The first token of a class should be \"class\"."))
    (if (eql 'identifier (token-type (second token-list)))
	(nconc parsed-list (list (concatenate 'string
				  "<identifier> "
				  (second token-list)
				  " </identifier>")))
	(error "The second token of a class should be an identifier."))
    (if (string= #\{ (third token-list))
	(nconc parsed-list (list "<symbol> { </symbol>"))
	(error "The third token of a class should be {."))

    (setf token-list (cdddr token-list))
    ;; Check if the next structure is classVarDec
    (loop while (member (first token-list) '("static" "field") :test 'string=)
	  do (multiple-value-bind (new-token-list new-parsed-list)
		 (compile-class-var-dec token-list parsed-list)
	       (setf token-list new-token-list)
	       (setf parsed-list new-parsed-list)))
    ;; Check if the next structure is subroutineDec
    (loop while (member (first token-list) '("constructor" "function" "method") :test 'string=)
	  do (multiple-value-bind (new-token-list new-parsed-list)
		 (compile-subroutine token-list parsed-list)
	       (setf token-list new-token-list)
	       (setf parsed-list new-parsed-list)))

    (if (string= #\} (first token-list))
	(nconc parsed-list (list "<symbol> } </symbol>"))
	(error "The last token of a class should be }."))
    (setf parsed-list (append parsed-list (list "</class>")))))

(defun compile-class-var-dec (token-list parsed-list)
  "Structure: ('static' | 'field') type varName (',' varName)* ';'"
  (nconc parsed-list (list (concatenate 'string "<keyword> " (first token-list) " </keyword>")))
  (if (is-type (second token-list))
      (if (eql 'identifier (token-type (second token-list)))
	  (nconc parsed-list (list (concatenate 'string
						"<identifier> "
						(second token-list)
						" </identifier>")))
	  (nconc parsed-list (list (concatenate 'string
						"<keyword> "
						(second token-list)
						" </keyword>"))))
      (error "The second token of a classVarDec should be a keyword."))
  
  (if (eql 'identifier (token-type (third token-list)))
      (nconc parsed-list (list (concatenate 'string
					    "<identifier> "
					    (third token-list)
					    " </identifier>")))
      (error "The third token of a classVarDec should be an identifier."))
  (setf token-list (cdddr token-list))
  (loop while (and (string= "," (first token-list))
		   (eql 'identifier (token-type (second token-list))))
	do
	   (nconc parsed-list (list "<symbol> , </symbol>"
				    (concatenate 'string "<identifier> "
						 (second token-list)
						 " </identifier>")))
	   (setf token-list (cddr token-list)))
  (if (string= ";" (first token-list))
      (nconc parsed-list '("<symbol> ; </symbol>"))
      (error "The last token of a classVarDec should be ';'."))
  (values (rest token-list) parsed-list))

(defun is-type (token)
  "Check if the token is a type."
  (or (member token '("int" "char" "boolean") :test 'string=)
      (eql 'identifier (token-type token))))

(defun compile-subroutine (token-list parsed-list)
  "Structure: ('constructor' | 'function' | 'method') ('void' | type) subroutineName '(' parameterList ')' subroutineBody"
  (nconc parsed-list (list (concatenate 'string "<keyword> " (first token-list) " </keyword>")))
  (if (or (string= "void" (second token-list)) (is-type (second token-list)))
      (if (eql 'identifier (token-type (second token-list)))
	  (nconc parsed-list (list (concatenate 'string
						"<identifier> "
						(second token-list)
						" </identifier>")))
	  (nconc parsed-list (list (concatenate 'string
						"<keyword> "
						(second token-list)
						" </keyword>"))))
      (error "The second token of subroutineDec should be type or 'void'."))
  (if (eql 'identifier (token-type (third token-list)))
      (nconc parsed-list (list (concatenate 'string
					    "<identifier> "
					    (third token-list)
					    " </identifier>")))
      (error "The third token of subroutineDec should be an identifier."))
  (setf token-list (cdddr token-list))
  (if (string= "(" (first token-list))
      (nconc parsed-list '("<symbol> ( </symbol>"))
      (error "Missing a '(' before parameterList."))
  (setf token-list (rest token-list))
  (when (is-type (first token-list))
      (multiple-value-bind (new-token-list new-parsed-list)
	  (compile-parameter-list token-list parsed-list)
	(setf token-list new-token-list)
	(setf parsed-list new-parsed-list)))
  (if (string= ")" (first token-list))
      (nconc parsed-list '("<symbol> ) </symbol>"))
      (error "Missing a ')' after parameterList."))
  (if (string= "{" (second token-list))
      (nconc parsed-list '("<symbol> { </symbol>"))
      (error "Missing a '{' before subroutineBody."))
  (setf token-list (cddr token-list))
  ;; varDec
  (loop while (string= "var" (first token-list))
	do (multiple-value-bind (new-token-list new-parsed-list)
	       (compile-var-dec token-list parsed-list)
	     (setf token-list new-token-list)
	     (setf parsed-list new-parsed-list)))
  ;; statements
  (loop while (member (first token-list) '("let" "if" "while" "do" "return") :test 'string=)
	do (multiple-value-bind (new-token-list new-parsed-list)
	       (compile-statements token-list parsed-list)
	     (setf token-list new-token-list)
	     (setf parsed-list new-parsed-list)))
  (if (string= "}" (first token-list))
      (nconc parsed-list '("<symbol> } </symbol>"))
      (error "The end of a subroutineBody should be '}'.")))

(defun compile-parameter-list (token-list parsed-list))

(defun compile-var-dec (token-list parsed-list))

(defun compile-statements (token-list parsed-list))
