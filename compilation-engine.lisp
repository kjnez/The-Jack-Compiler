(in-package #:compiler)


(defun compile-class (token-list)
  "The highest level parser for each class. Returns a new token-list and a parsed-list.
   Structure: 'class' className '{' classVarDec* subroutineDec* '}'"
  (defparameter *h* (make-symbol-table))
  (let ((parsed-list (list))
	(class-name (second token-list)))
    (if (string= "class" (first token-list))
	(setf parsed-list (list "<class>" "<keyword> class </keyword>"))
	(error "The first token of a class should be \"class\"."))
    (if (eql 'identifier (token-type (second token-list))) ; class name
	(nconc parsed-list (list (concatenate 'string
					      "<identifier> "
					      (second token-list)
					      " #\# class defined nil"
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
	  do (start-subroutine *h*)
	     (define *h* "this" class-name "arg")
	     (multiple-value-bind (new-token-list new-parsed-list)
		 (compile-subroutine token-list parsed-list)
	       (setf token-list new-token-list)
	       (setf parsed-list new-parsed-list)))
    (if (string= #\} (first token-list))
	(setf parsed-list (append parsed-list (list "<symbol> } </symbol>")))
	(error "The last token of a class should be }."))
    (append parsed-list (list "</class>"))))

(defun compile-class-var-dec (token-list &optional (parsed-list '()))
  "Structure: ('static' | 'field') type varName (',' varName)* ';'"
  (let ((appendent (list "<classVarDec>"))
	(type (second token-list))
	(kind (first token-list)))
    (nconc appendent (list (concatenate 'string "<keyword> " (first token-list) " </keyword>")))
    (if (is-type (second token-list))
	(if (eql 'identifier (token-type (second token-list)))
	    (nconc appendent (list (concatenate 'string
						"<identifier> "
						(second token-list)
						" #\# class used nil"
						" </identifier>")))
	    (nconc appendent (list (concatenate 'string
						"<keyword> "
						(second token-list)
						" </keyword>"))))
	(error "The second token of a classVarDec should be a keyword."))
    (if (eql 'identifier (token-type (third token-list)))
	(progn
	  (define *h* (third token-list) type kind)
	  (nconc appendent (list (concatenate 'string
					      "<identifier> "
					      (third token-list)
					      " #\# " (first token-list) " defined t "
					      (write-to-string (index-of *h* (third token-list)))
					      " </identifier>"))))
	(error "The third token of a classVarDec should be an identifier."))
    (let ((rest-tokens (cdddr token-list)))
      (loop while (and (string= "," (first rest-tokens))
		       (eql 'identifier (token-type (second rest-tokens))))
	    do
	       (define *h* (second rest-tokens) type kind)
	       (nconc appendent (list "<symbol> , </symbol>"
				      (concatenate 'string "<identifier> "
						   (second rest-tokens)
						   " #\# " kind " defined t "
						   (write-to-string (index-of *h* (second rest-tokens)))
						   " </identifier>")))
	       (setf rest-tokens (cddr rest-tokens)))
      (if (string= ";" (first rest-tokens))
	  (nconc appendent (list "<symbol> ; </symbol>"))
	  (error "The last token of a classVarDec should be ';'."))
      (values (rest rest-tokens) (append parsed-list appendent (list "</classVarDec>"))))))

(defun is-type (token)
  "Check if the token is a type."
  (or (member token (list "int" "char" "boolean") :test 'string=)
      (eql 'identifier (token-type token))))

(defun is-keyword-type (token)
  "Check if the token is ('int' | 'char' | boolean')."
  (member token (list "int" "char" "boolean") :test 'string=))

(defun compile-subroutine (token-list &optional (parsed-list '()))
  "Structure: ('constructor' | 'function' | 'method') ('void' | type) subroutineName '(' parameterList ')' subroutineBody"
  (let ((appendent (list "<subroutineDec>"))
	(type (second token-list)))
    (nconc appendent (list (concatenate 'string "<keyword> " (first token-list) " </keyword>")))
    (if (or (string= "void" (second token-list)) (is-type (second token-list)))
	(if (eql 'identifier (token-type (second token-list)))
	    (nconc appendent (list (concatenate 'string
						"<identifier> "
						(second token-list)
						" #\# class used nil"
						" </identifier>")))
	    (nconc appendent (list (concatenate 'string
						"<keyword> "
						(second token-list)
						" </keyword>"))))
	(error "The second token of subroutineDec should be type or 'void'."))
    (if (eql 'identifier (token-type (third token-list)))
	(progn
	  (nconc appendent (list (concatenate 'string
					      "<identifier> "
					      (third token-list)
					      " #\# subroutine defined nil"
					      " </identifier>"))))
	(error "The third token of subroutineDec should be an identifier."))
    (let ((rest-tokens (cdddr token-list)))
      (if (string= "(" (first rest-tokens))
	  (nconc appendent (list "<symbol> ( </symbol>"))
	  (error "Missing a '(' before parameterList."))
      (setf rest-tokens (rest rest-tokens))
      (multiple-value-bind (new-rest-tokens new-appendent)
	  (compile-parameter-list rest-tokens appendent)
	(setf rest-tokens new-rest-tokens)
	(setf appendent new-appendent))
      (if (string= ")" (first rest-tokens))
	  (nconc appendent (list "<symbol> ) </symbol>"))
	  (error "Missing a ')' after parameterList."))
      (if (string= "{" (second rest-tokens))
	  (nconc appendent (list "<subroutineBody>" "<symbol> { </symbol>")) ;; wrong before here
	  (error "Missing a '{' before subroutineBody."))
      (setf rest-tokens (cddr rest-tokens))
      ;; varDec
      (loop while (string= "var" (first rest-tokens))
	    do (multiple-value-bind (new-rest-tokens new-appendent)
		   (compile-var-dec rest-tokens appendent)
		 (setf rest-tokens new-rest-tokens)
		 (setf appendent new-appendent)))
      (multiple-value-bind (new-rest-tokens new-appendent)
	  (compile-statements rest-tokens appendent)
	(setf rest-tokens new-rest-tokens)
	(setf appendent new-appendent))
      (if (string= "}" (first rest-tokens))
	  (setf appendent (append appendent
				  (list "<symbol> } </symbol>" "</subroutineBody>")))
	  (error "The end of a subroutineBody should be '}'."))
      (values (rest rest-tokens) (append parsed-list appendent (list "</subroutineDec>"))))))

(defun compile-parameter-list (token-list &optional (parsed-list '()))
  "Structure: ((type varName) (',' type varName)*)?"
  (let ((appendent (list "<parameterList>")))
    (loop while (is-type (first token-list))
	  do (define *h* (second token-list) (first token-list) "arg")
	     (if (is-keyword-type (first token-list))
		 (nconc appendent
			(list (concatenate 'string "<keyword> "
					   (first token-list)
					   " </keyword>"))
			(list (concatenate 'string "<identifier> "
					   (second token-list)
					   " #\# arg defined t "
					   (write-to-string (index-of *h* (second token-list)))
					   " </identifier>")))
		 (nconc appendent
			(list (concatenate 'string "<identifier> "
					   (first token-list)
					   " #\# class used nil"
					   " </identifier>")
			      (concatenate 'string "<identifier> "
					   (second token-list)
					   " #\# arg defined t "
					   (write-to-string (index-of *h* (second token-list)))
					   " </identifier>"))))
	     (cond ((string= #\, (third token-list))
		    (nconc appendent (list "<symbol> , </symbol>"))
		    (setf token-list (cdddr token-list)))
		   ((string= #\) (third token-list))
		    (setf token-list (cddr token-list)))
		   (t
		    (print appendent)
		    (error "There's an error in the parameterList."))))
    (values token-list (append parsed-list appendent (list "</parameterList>")))))

(defun compile-var-dec (token-list &optional (parsed-list '()))
  "Structure: 'var' type varName (',' varName)* ';'"
  (let ((appendent (list "<varDec>" "<keyword> var </keyword>"))
	(type (second token-list)))
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
						   " #\# class used nil"
						   " </identifier>"))))
	      (t (error "Type in varDec should be keyword or identifier.")))
	(error "Second element of varDec should be type."))
    (if (eql 'identifier (token-type (third token-list)))
	(progn
	  (define *h* (third token-list) (second token-list) "var")
	  (nconc appendent (list (concatenate 'string
					      "<identifier> "
					      (third token-list)
					      " #\# var defined t "
					      (write-to-string (index-of *h* (third token-list)))
					      " </identifier>"))))
	(error "Third element of varDec should be an identifier."))
    (let ((rest-tokens (cdddr token-list)))
      (loop while (string= #\, (first rest-tokens))
	    do (nconc appendent (list "<symbol> , </symbol>"))
	       (if (eql 'identifier (token-type (second rest-tokens)))
		   (progn
		     (define *h* (second rest-tokens) type "var")
		     (nconc appendent (list (concatenate 'string
							"<identifier> "
							(second rest-tokens)
							" #\# var defined t "
							(write-to-string (index-of *h* (second rest-tokens)))
							" </identifier>"))))
		   (error "varName should be an identifier."))
	       (setf rest-tokens (cddr rest-tokens)))
      (if (string= #\; (first rest-tokens))
	  (values (cdr rest-tokens) (append parsed-list appendent
					    (list "<symbol> ; </symbol>" "</varDec>")))
	  (error "Last element of varDec should be ;.")))))

(defun compile-statement (token-list &optional (parsed-list '()))
  "Structure: (let | if | while | do | return)"
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

(defun compile-statements (token-list &optional (parsed-list '()))
  (let ((appendent (list "<statements>"))
	(rest-tokens token-list))
    (loop while (member (first rest-tokens) '("let" "if" "while" "do" "return") :test 'string=)
	  do (multiple-value-bind (new-rest-tokens new-appendent)
		 (compile-statement rest-tokens appendent)
	       (setf rest-tokens new-rest-tokens)
	       (setf appendent new-appendent)))
    (values rest-tokens (append parsed-list appendent '("</statements>")))))

(defun compile-let (token-list &optional (parsed-list '()))
  "Structure: 'let' varName ('[' expression ']')? '=' expression ';'"
  (let ((appendent (list "<letStatement>" "<keyword> let </keyword>")))
    (if (eql 'identifier (token-type (second token-list)))
	(nconc appendent (list (concatenate 'string
					    "<identifier> "
					    (second token-list)
					    " #\# var used t "
					    (write-to-string (index-of *h* (second token-list)))
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
      (nconc appendent (list "<symbol> = </symbol>"))
      ;; (assert (eql 'identifier (token-type (second rest-tokens))))
      (multiple-value-bind (new-token-list new-appendent)
	  (compile-expression (rest rest-tokens) appendent)
	(setf rest-tokens new-token-list)
	(setf appendent new-appendent))
      (assert (string= #\; (first rest-tokens)))
      (values (rest rest-tokens)
	      (append parsed-list appendent
		      (list "<symbol> ; </symbol>" "</letStatement>"))))))

(defun compile-do (token-list &optional (parsed-list '()))
  "Structure: 'do' subroutineCall ';'"
  (let ((appendent (list "<doStatement>" "<keyword> do </keyword>")))
    (multiple-value-bind (new-token-list new-appendent)
	(compile-subroutine-call (rest token-list) appendent)
      (assert (string= #\; (first new-token-list)))
      (values (rest new-token-list) (append parsed-list new-appendent
					    (list "<symbol> ; </symbol>") (list "</doStatement>"))))))

(defun compile-while (token-list &optional (parsed-list '()))
  "Structure: 'while' '(' expression ')' '{' statements '}'"
  (let ((appendent (list "<whileStatement>" "<keyword> while </keyword>")))
    (assert (string= #\( (second token-list)))
    (nconc appendent (list "<symbol> ( </symbol>"))
    (let ((rest-tokens (cddr token-list)))
      (multiple-value-bind (new-token-list new-appendent)
	  (compile-expression rest-tokens appendent)
	(setf rest-tokens new-token-list)
	(setf appendent new-appendent))
      (assert (string= #\) (first rest-tokens)))
      (nconc appendent (list "<symbol> ) </symbol>"))
      (assert (string= #\{ (second rest-tokens)))
      (nconc appendent (list "<symbol> { </symbol>"))
      (multiple-value-bind (new-token-list new-appendent)
	  (compile-statements (cddr rest-tokens) appendent)
	(setf rest-tokens new-token-list)
	(setf appendent new-appendent))
      (assert (string= #\} (first rest-tokens)))
      ;; (nconc appendent (list "<symbol> } </symbol>"))
      (values (cdr rest-tokens) (append parsed-list appendent
					(list "<symbol> } </symbol>")
					(list "</whileStatement>"))))))

(defun compile-return (token-list &optional (parsed-list '()))
  "Structure: 'return' expression? ';'"
  ;; (declare (optimize debug))
  (let ((appendent (list "<returnStatement>" "<keyword> return </keyword>"))
	(rest-tokens token-list))
    ;; for now assume expression is an identifier
    (if (is-term (second rest-tokens))
	(multiple-value-bind (new-token-list new-appendent)
	    (compile-expression (rest rest-tokens) appendent)
	  (setf rest-tokens new-token-list)
	  (setf appendent new-appendent))
	(setf rest-tokens (rest rest-tokens)))
    (assert (string= #\; (first rest-tokens)))
    (values (rest rest-tokens) (append parsed-list appendent
				       (list "<symbol> ; </symbol>" "</returnStatement>")))))

(defun compile-if (token-list &optional (parsed-list '()))
  "Structure: 'if' '(' expression ')' '{' statements '}'"
  (let ((appendent (list "<ifStatement>" "<keyword> if </keyword>"))
	(rest-tokens token-list))
    (assert (string= "if" (first rest-tokens)))
    (assert (string= #\( (second rest-tokens)))
    (nconc appendent (list "<symbol> ( </symbol>"))
    (multiple-value-bind (new-rest-tokens new-appendent)
	(compile-expression (cddr rest-tokens) appendent)
      (setf rest-tokens new-rest-tokens)
      (setf appendent new-appendent))
    (assert (string= #\) (first rest-tokens)))
    (nconc appendent (list "<symbol> ) </symbol>"))
    (assert (string= #\{ (second rest-tokens)))
    (nconc appendent (list "<symbol> { </symbol>"))
    (multiple-value-bind (new-rest-tokens new-appendent)
	(compile-statements (cddr rest-tokens) appendent)
      (setf rest-tokens new-rest-tokens)
      (setf appendent new-appendent))
    (assert (string= #\} (first rest-tokens)))
    (setf appendent (append appendent (list "<symbol> } </symbol>")))
    (setf rest-tokens (rest rest-tokens))
    (when (string= "else" (first rest-tokens))
      (nconc appendent (list "<keyword> else </keyword>"))
      (assert (string= #\{ (second rest-tokens)))
      (nconc appendent (list "<symbol> { </symbol>"))
      (multiple-value-bind (new-rest-tokens new-appendent)
	  (compile-statements (cddr rest-tokens) appendent)
	(setf rest-tokens new-rest-tokens)
	(setf appendent new-appendent))
      (assert (string= #\} (first rest-tokens)))
      (setf appendent (append appendent (list "<symbol> } </symbol>")))
      (setf rest-tokens (rest rest-tokens)))
    (values rest-tokens (append parsed-list appendent (list "</ifStatement>")))))

(defun is-op (token)
  "Return t if token is an op."
  (when (member token '("+" "-" "*" "/" "&" "|" "<" ">" "=") :test 'string=)
    t))

(defun normalize-op (token)
  "Change '<' to '&lt;', '>' to '&gt;', '&' to '&amp;'."
  (cond ((string= #\< token)
	 "&lt;")
	((string= #\> token)
	 "&gt;")
	((string= #\& token)
	 "&amp;")
	(t token)))

(defun compile-expression (token-list &optional (parsed-list '()))
  "Structure: term (op term)*"
  (let ((appendent (list)))
    (when (is-term (first token-list))
      (setf appendent (list "<expression>"))
      (multiple-value-bind (new-rest-tokens new-appendent)
	  (compile-term token-list appendent)
	(setf token-list new-rest-tokens)
	(setf appendent new-appendent))
      (loop while (is-op (first token-list))
	    do (nconc appendent (list (concatenate 'string
						   "<symbol> "
						   (normalize-op (string (first token-list)))
						   " </symbol>")))
	       (multiple-value-bind (new-rest-tokens new-appendent)
		   (compile-term (rest token-list) appendent)
		 (setf token-list new-rest-tokens)
		 (setf appendent new-appendent)))
      (nconc appendent (list "</expression>")))
    (values token-list (append parsed-list appendent))))

(defun is-term (token)
  "Returns t if the next (several) element(s) is(are) term(s)."
  (or (eql 'int-const (token-type token))
      (eql 'string-const (token-type token))
      (eql 'keyword (token-type token))
      (string= #\( token)
      (member token '("-" "~") :test 'string=)
      (eql 'identifier (token-type token))))

(defun compile-term (token-list &optional (parsed-list '()))
  "Structure: integerConstant | stringConstant | keywordConstant | varName | varName '[' expression ']' | subroutineCall | '(' expression ')' | unaryOp term"
  (let ((appendent (list "<term>")))
    (cond ((eql 'int-const (token-type (first token-list)))
	   (nconc appendent (list (concatenate 'string "<integerConstant> "
					       (first token-list)
					       " </integerConstant>")))
	   (setf token-list (rest token-list)))
	  ((eql 'string-const (token-type (first token-list)))
	   (nconc appendent (list (concatenate 'string "<stringConstant> "
					       (subseq (first token-list) 1 (1- (length (first token-list))))
					       " </stringConstant>")))
	   (setf token-list (rest token-list)))
	  ((eql 'keyword (token-type (first token-list)))
	   (nconc appendent (list (concatenate 'string "<keyword> "
					       (first token-list)
					       " </keyword>")))
	   (setf token-list (rest token-list)))
	  ((string= #\( (first token-list))
	   (nconc appendent (list "<symbol> ( </symbol>"))
	   (multiple-value-bind (new-rest-tokens new-appendent)
	       (compile-expression (rest token-list) appendent)
	     (setf token-list new-rest-tokens)
	     (setf appendent new-appendent))
	   (assert (string= #\) (first token-list)))
	   (setf token-list (rest token-list))
	   (nconc appendent (list "<symbol> ) </symbol>")))
	  ((eql 'identifier (token-type (first token-list)))
	   (cond ((string= #\[ (second token-list))
		  (nconc appendent
			 (list (concatenate 'string "<identifier> "
					    (first token-list)
					    " #\# var used t "
					    (write-to-string (index-of *h* (first token-list)))
					    " </identifier>"))
			 (list "<symbol> [ </symbol>"))
		  (multiple-value-bind (new-rest-tokens new-appendent)
		      (compile-expression (cddr token-list) appendent)
		    (setf token-list new-rest-tokens)
		    (setf appendent new-appendent))
		  (assert (string= #\] (first token-list)))
		  (nconc appendent (list "<symbol> ] </symbol>"))
		  (setf token-list (rest token-list)))
		 ((or (string= #\( (second token-list))
		      (string= #\. (second token-list)))
		  (multiple-value-bind (new-rest-tokens new-appendent)
		      (compile-subroutine-call token-list appendent)
		    (setf token-list new-rest-tokens)
		    (setf appendent new-appendent)))
		 (t (nconc appendent (list (concatenate 'string "<identifier> "
							(first token-list)
							" #\# var used t "
							(write-to-string (index-of *h* (first token-list)))
							" </identifier>")))
		    (setf token-list (rest token-list)))))
	  ((member (first token-list) '("-" "~") :test 'string=)
	   (nconc appendent (list (concatenate 'string "<symbol> "
					       (normalize-unary-op (first token-list))
					       " </symbol>")))
	   (multiple-value-bind (new-rest-tokens new-appendent)
	       (compile-term (rest token-list) appendent)
	     (setf token-list new-rest-tokens)
	     (setf appendent new-appendent)))
	  (t (error "Invalid term.")))
    (values token-list (append parsed-list appendent (list "</term>")))))

(defun normalize-unary-op (token)
  (if (string= #\~ token)
      "~~"
      (string token)))

(defun compile-subroutine-call (token-list &optional (parsed-list '()))
  "Structure: subroutineName '(' expressionList ')' | (className | varName) '.' subroutineName '(' expressionList ')'"
  (let ((appendent (list))
	(rest-tokens token-list))
    (assert (eql 'identifier (token-type (first rest-tokens))))
    (cond ((string= #\( (second rest-tokens))
	   (setf appendent (list (concatenate 'string "<identifier> "
					      (first rest-tokens)
					      " #\# subroutine used nil"
					      " </identifier>")))
	   (nconc appendent (list "<symbol> ( </symbol>"))
	   (multiple-value-bind (new-rest-tokens new-appendent)
	       (compile-expression-list (cddr rest-tokens) appendent)
	     (setf rest-tokens new-rest-tokens)
	     (setf appendent new-appendent))
	   (assert (string= #\) (first rest-tokens)))
	   (nconc appendent (list "<symbol> ) </symbol>")))
	  ((string= #\. (second rest-tokens))
	   (if (string= "var" (kind-of *h* (first rest-tokens)))
	       ;; var name
	       (setf appendent (list (concatenate 'string "<identifier> "
						  (first rest-tokens)
						  " #\# var used t "
						  (write-to-string (index-of *h* (first rest-tokens)))
						  " </identifier>")))
	       ;; class name
	       (setf appendent (list (concatenate 'string "<identifier> "
						  (first rest-tokens)
						  " #\# class used nil"
						  " </identifier>"))))
	   (nconc appendent (list "<symbol> . </symbol>"))
	   (assert (eql 'identifier (token-type (third rest-tokens))))
	   (nconc appendent (list (concatenate 'string "<identifier> "
					       (third rest-tokens)
					       " #\# subroutine used nil"
					       " </identifier>")))
	   (assert (string= #\( (fourth rest-tokens)))
	   (nconc appendent (list "<symbol> ( </symbol>"))
	   (multiple-value-bind (new-rest-tokens new-appendent)
	       (compile-expression-list (nthcdr 4 rest-tokens) appendent)
	     (setf rest-tokens new-rest-tokens)
	     (setf appendent new-appendent))
	   (assert (string= #\) (first rest-tokens)))
	   (nconc appendent (list "<symbol> ) </symbol>")))
	  (t (error "Something is wrong with the subroutine calling.")))
    (values (cdr rest-tokens) (append parsed-list appendent))))

(defun compile-expression-list (token-list &optional (parsed-list '()))
  "Structure: (expression (',' expression)*)?"
  (let ((appendent (list "<expressionList>"))
	(rest-tokens token-list))
    (multiple-value-bind (new-rest-tokens new-appendent)
	(compile-expression rest-tokens appendent)
      (setf rest-tokens new-rest-tokens)
      (setf appendent new-appendent))
    (loop while (string= #\, (first rest-tokens))
	  do (nconc appendent (list "<symbol> , </symbol>"))
	     (multiple-value-bind (new-rest-tokens new-appendent)
		 (compile-expression (rest rest-tokens) appendent)
	       (setf rest-tokens new-rest-tokens)
	       (setf appendent new-appendent)))
    (values rest-tokens (append parsed-list appendent (list "</expressionList>")))))

(defun jack-parser (tokenizer file stream)
  "Tokenize the file, then write the parsed results to stream."
  (let* ((tokenized-list (funcall tokenizer file))
	 (parsed-list (compile-class tokenized-list)))
    (dolist (line parsed-list)
      (format stream (concatenate 'string line "~%")))))

(defun parser-writer (dir &optional (tokenizer #'tokenizer))
  (let* ((jack-files (directory (concatenate 'string dir "/*.jack")))
	 (output-folder (concatenate 'string dir "/parsed/")))
    (dolist (jack-file jack-files)
      (let ((xml-file (concatenate 'string output-folder (pathname-name jack-file) ".xml")))
	(with-open-file (stream xml-file :direction :output :if-exists :supersede)
	  (jack-parser tokenizer jack-file stream))))))

;; Unit tests
(defun compile-var-dec-test ()
  (compile-var-dec (list "var" "int" "bar" "," "foo" ";") '()))

(defun compile-expression-test ()
  (compile-expression (list "bar" "+" "barz" "-" "foo") '()))

(defun compile-let-test ()
  (compile-let (list "let" "tmp" "=" "foo" ";") '()))

(defun compile-return-test ()
  (compile-return (list "return" "foo" ";") '()))

(defun compile-statements-test ()
  (compile-statements '("let" "temp" "=" "x" ";" "let" "foo" "=" "bar" ";") '()))

(defun compile-if-test ()
  (compile-if (list "if" "(" "foo" ")" "{" "return" "zoo" ";" "}" "else" "{" "let" "foo" "=" "bar" ";" "}")))

(defun compile-expression-list-test ()
  (compile-expression-list (list "kos" "," "nko" "," "oii")))

(defun compile-subroutine-call-test ()
  ;; (compile-subroutine-call '("did" "(" ")"))
  (compile-subroutine-call '("did" "(" "expr" ")" "." "expr2" "(" ")")))

(defun compile-do-test ()
  (compile-do '("do" "func" "(" "expr" ")" "." "expr2" "(" ")" ";")))


;; (parser-writer "~/nand2tetris/projects/10/Square/" #'tokenizer)

