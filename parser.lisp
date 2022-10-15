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
    (append parsed-list (list "</class>"))))

(defun compile-class-var-dec (token-list parsed-list)
  "Structure: ('static' | 'field') type varName (',' varName)* ';'"
  (let ((appendent '()))
    (setf appendent (list (concatenate 'string "<keyword> " (first token-list) " </keyword>")))
    (if (is-type (second token-list))
	(if (eql 'identifier (token-type (second token-list)))
	    (nconc appendent (list (concatenate 'string
						"<identifier> "
						(second token-list)
						" </identifier>")))
	    (nconc appendent (list (concatenate 'string
						"<keyword> "
						(second token-list)
						" </keyword>"))))
	(error "The second token of a classVarDec should be a keyword."))
    
    (if (eql 'identifier (token-type (third token-list)))
	(nconc appendent (list (concatenate 'string
					    "<identifier> "
					    (third token-list)
					    " </identifier>")))
	(error "The third token of a classVarDec should be an identifier."))
    (let ((rest-tokens (cdddr token-list)))
     (loop while (and (string= "," (first rest-tokens))
		      (eql 'identifier (token-type (second rest-tokens))))
	   do
	      (nconc appendent (list "<symbol> , </symbol>"
				     (concatenate 'string "<identifier> "
						  (second rest-tokens)
						  " </identifier>")))
	      (setf rest-tokens (cddr rest-tokens)))
      (if (string= ";" (first rest-tokens))
	  (nconc appendent '("<symbol> ; </symbol>"))
	  (error "The last token of a classVarDec should be ';'."))
      (values (rest rest-tokens) (append parsed-list appendent)))))

(defun is-type (token)
  "Check if the token is a type."
  (or (member token '("int" "char" "boolean") :test 'string=)
      (eql 'identifier (token-type token))))

(defun compile-subroutine (token-list parsed-list)
  "Structure: ('constructor' | 'function' | 'method') ('void' | type) subroutineName '(' parameterList ')' subroutineBody"
  (let ((appendent '()))
    (setf appendent (list (concatenate 'string "<keyword> " (first token-list) " </keyword>")))
    (if (or (string= "void" (second token-list)) (is-type (second token-list)))
	(if (eql 'identifier (token-type (second token-list)))
	    (nconc appendent (list (concatenate 'string
						"<identifier> "
						(second token-list)
						" </identifier>")))
	    (nconc appendent (list (concatenate 'string
						"<keyword> "
						(second token-list)
						" </keyword>"))))
	(error "The second token of subroutineDec should be type or 'void'."))
    (if (eql 'identifier (token-type (third token-list)))
	(nconc appendent (list (concatenate 'string
					    "<identifier> "
					    (third token-list)
					    " </identifier>")))
	(error "The third token of subroutineDec should be an identifier."))
    (let ((rest-tokens (cdddr token-list)))
     (if (string= "(" (first rest-tokens))
	 (nconc appendent '("<symbol> ( </symbol>"))
	 (error "Missing a '(' before parameterList."))
      (setf rest-tokens (rest rest-tokens))
      (when (is-type (first rest-tokens))
	(multiple-value-bind (new-rest-tokens new-appendent)
	    (compile-parameter-list rest-tokens appendent)
	  (setf rest-tokens new-rest-tokens)
	  (setf appendent new-appendent)))
      (if (string= ")" (first rest-tokens))
	  (nconc appendent '("<symbol> ) </symbol>"))
	  (error "Missing a ')' after parameterList."))
      (if (string= "{" (second rest-tokens))
	  (nconc appendent '("<symbol> { </symbol>"))
	  (error "Missing a '{' before subroutineBody."))
      (setf rest-tokens (cddr rest-tokens))
      ;; varDec
      (loop while (string= "var" (first rest-tokens))
	    do (multiple-value-bind (new-rest-tokens new-appendent)
		   (compile-var-dec rest-tokens appendent)
		 (setf rest-tokens new-rest-tokens)
		 (setf appendent new-appendent)))
      ;; statements
      (loop while (member (first rest-tokens) '("let" "if" "while" "do" "return") :test 'string=)
	    do (multiple-value-bind (new-rest-tokens new-appendent)
		   (compile-statements rest-tokens appendent)
		 (setf rest-tokens new-rest-tokens)
		 (setf appendent new-appendent)))
      (if (string= "}" (first rest-tokens))
	  (nconc appendent '("<symbol> } </symbol>"))
	  (error "The end of a subroutineBody should be '}'."))
      (values (rest rest-tokens) (append parsed-list appendent)))))

(defun compile-parameter-list (token-list parsed-list)
  "Structure: ((type varName) (',' type varName)*)?"
  (let ((appendent '()))
    (if (eql 'identifier (token-type (second token-list)))
	(if (eql 'identifier (token-type (first token-list)))
	    (setf appendent
		  (append (list (concatenate 'string "<keyword> "
					     (first token-list)
					     " </keyword>"))
			  (list (concatenate 'string "<identifier> "
					     (second token-list)
					     " </identifier>"))))
	    (setf appendent
		  (append (list (concatenate 'string "<identifier> "
					     (first token-list)
					     " </identifier>"))
			  (list (concatenate 'string "<identifier> "
					     (second token-list)
					     " </identifier>")))))
	(error "The second element of a parameterList should be an identifier."))
    (if (not (string= #\, (third token-list)))
	(values (cddr token-list) (append parsed-list appendent))
	(progn
	  (compile-parameter-list
	   (cdddr token-list)
	   (append parsed-list
		   appendent
		   (list (concatenate 'string "<symbol> , </symbol>"))))))))

(defun compile-var-dec (token-list parsed-list)
  "Structure: 'var' type varName (',' varName)* ';'"
  (let ((appendent (list "<keyword> var </keyword>")))
    (assert (eql 'keyword (token-type (first token-list))))
    (if (is-type (second token-list))
	(cond ((eql 'keyword (token-type (second token-list)))
	       (nconc appendent (list (concatenate 'string
						   "<keyword> "
						   (second token-list)
						   " </keyword>"))))
	      ((eql 'identifier (token-type (second token-list)))
	       (nconc appendent (list (concatenate 'string
						   "<identifier> "
						   (second token-list)
						   " </identifier>"))))
	      (t (error "Type in varDec should be keyword or identifier.")))
	(error "Second element of varDec should be type."))
    (if (eql 'identifier (token-type (third token-list)))
	(nconc appendent (list (concatenate 'string
					    "<identifier> "
					    (third token-list)
					    " </identifier>")))
	(error "Third element of varDec should be an identifier."))
    (let ((rest-tokens (cdddr token-list)))
      (loop while (string= #\, (first rest-tokens))
	    do (nconc appendent (list "<symbol> , </symbol>"))
	       (if (eql 'identifier (token-type (second rest-tokens)))
		   (nconc appendent (list (concatenate 'string
						       "<identifier> "
						       (second rest-tokens)
						       " </identifier>")))
		   (error "varName should be an identifier."))
	       (setf rest-tokens (cddr rest-tokens)))
      (if (string= #\; (first rest-tokens))
	  (values (cdr rest-tokens) (append parsed-list appendent (list "<symbol> ; </symbol>")))
	  (error "Last element of varDec should be ;.")))))

(defun compile-statement (token-list parsed-list)
  "Structure: (let | if | while | do | return)*"
  (let ((first-element (first token-list)))
    (cond ((string= "let" first-element)
	   (compile-let token-list parsed-list))
	  ((string= "if" first-element)
	   (compile-if token-list parsed-list))
	  ((string= "while" first-element)
	   (compile-while token-list parsed-list))
	  ((string= "do" first-element)
	   (compile-do token-list parsed-list))
	  ((string= "return" first-element)
	   (compile-return token-list parsed-list))
	  (t (error "Statement not recognized.")))))

(defun compile-let (token-list parsed-list)
  "Structure: 'let' varName ('[' expression ']')? '=' expression ';'"
  (let ((appendent (list "<keyword> let </keyword>")))
    (if (eql 'identifier (token-type (second token-list)))
	(nconc appendent (list (concatenate 'string
					    "<identifier> "
					    (second token-list)
					    " </identifier>")))
	(error "varName should be an identifier."))
    (let ((rest-tokens (cddr token-list)))
      (when (string= #\[ (first rest-tokens))
	(nconc appendent (list "<symbol> [ </symbol>"))
	(multiple-value-bind (new-token-list new-appendent)
	    (compile-expression (rest rest-tokens) appendent)
	  (setf rest-tokens new-token-list)
	  (setf appendent new-appendent))
	(assert (string= #\] (first rest-tokens)))
	(nconc appendent (list "<symbol> ] </symbol>"))
	(setf rest-tokens (rest rest-tokens)))
      (assert (string= #\= (first rest-tokens)))
      (nconc appendent '("<symbol> = </symbol>"))
      (multiple-value-bind (new-token-list new-appendent)
	  (compile-expression (rest rest-tokens) appendent)
	(setf rest-tokens new-token-list)
	(setf appendent new-appendent))
      (assert (string= #\; (first rest-tokens)))
      (values (rest rest-tokens)
	      (append parsed-list appendent '("<symbol> ; </symbol>"))))))

(defun compile-do (token-list parsed-list)
  "Structure: 'do' subroutineCall ';'"
  (let ((appendent (list "<keyword> do </keyword>")))
    (multiple-value-bind (new-token-list new-appendent)
	(compile-subroutine-call (rest token-list) appendent)
      (assert (string= #\; (first new-token-list)))
      (values (rest new-token-list) (append parsed-list new-appendent
					    '("<symbol> ; </symbol>"))))))

(defun compile-while (token-list parsed-list)
  "Structure: 'while' '(' expression ')' '{' statements '}'")

(defun compile-return (token-list parsed-list))

(defun compile-if (token-list parsed-list))

(defun compile-expression (token-list parsed-list))

(defun compile-subroutine-call (token-list parsed-list))


;; Unit tests
(defun compile-var-dec-test ()
  (compile-var-dec (list "var" "int" "bar" "," "foo" ";") '()))
