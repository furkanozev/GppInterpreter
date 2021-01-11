; *********************************************
; *  341 Programming Languages                *
; *  Fall 2019                                *
; *  Author: Furkan Ozev                      *
; *  Number: 161044036						  *
; *********************************************

;; > clisp gpp_lexer.lisp
;; > (gppinterpreter)						for interpreter
;; > (gppinterpreter "filename.g++")		for load file by the interpreter and interpreted right away

;; Operator , Keywords
(defvar KeyWord (list "and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))
(defvar KW (list "KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))
(defvar Operator (list "+" "-" "/" "**" "*" "(" ")" "\"" "\"" "," "'"))
(defvar OP (list "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_DBLMULT" "OP_MULT" "OP_OP" "OP_CP" "OP_OC" "OP_CC" "OP_COMMA" "CUT"))

(defvar Space (list "\n" "\t" " "))
(defvar Comment ";")
(defvar Possible (list "(" ")" "\""))
(defvar opoc 0)

(defvar tokens (list))
(defvar tokensline (list))

(defvar symbol (list))
(defvar symbolval (list))
(defvar counterglob 1)

;;[a-zA-Z_][a-zA-Z0-9_]*			return IDENTIFIER;

;;[0]|[1-9][0-9]*					return VALUE;	

;; “gppinterpreter” function that will start interpreter.
;; “gppinterpreter” can have zero or one input.
;; The input can be a file name which will be loaded by the interpreter and interpreted right away.
(defun gppinterpreter (&optional (filename -1))
	(if (equal filename -1)
		(let ((line) (check))
			(loop
			   (format t "[~S]> " counterglob)
			   (setq line (read-line))
			   (setq check (evalline line))
			   (terpri)
			   (setq counterglob (+ counterglob 1))
			   (when (= check -1) (return))
			)
		)
		(let ((in (open filename :if-does-not-exist nil)))
   			(when in
      			(loop for line = (read-line in nil)
      
      			while line do (progn (evalline line) (terpri)))
      			(close in)
   			)
		)
	)
)

;; Take a line, then split with whitespace to split word.
;; After then, To detect token of word, call evalword function for each word.
(defun evalline (line)
	(let ((words) (res 0) (tempword) (check 0))
		(setq tokensline (list))
		(setq tokens (list))
		(setq line (string-trim '(#\Space #\Tab #\Newline) line))
		(setq words (split-str line))
		(loop for word in words
			do
			(progn
				(setq tempword (string-trim '(#\Space #\Tab #\Newline) word))
				(setq res (evalword tempword))
				(if (or (equal res 2) (equal res -1)) (return res))
			)
		)
		(if (equal res -1)
			(write "SYNTAX_ERROR Expression not recognized")
			(progn
				(if (equal res 2) ()
					(progn
						(setq check (evaluate))
						(if (equal check nil) (setq check (explisti)))
						(if (equal check nil) (write "SYNTAX_ERROR Expression not recognized"))
					)
				)
			)
		)
		res			
	)
)

;; For each word, this function determine token of word.
;; Reads the characters of the word in sequence. Therefore, a new subword is formed in each reading.
;; Checks if subword matches any token.
(defun evalword (word)
	(let ((len (length word)) (subword) (j 0) (res) (temp) (check 0) (id 0))
		(loop for i from 1 to len
			do
			(progn
				(if (= check 1) (setq check 0) )
				(setq subword (string-downcase (subseq word j i)))

				;; Check wheter subword is operator or not.
				(if (= check 0)
					(progn
						(setq res (findinList subword Operator))
						(if (not (equal res nil))
							(progn
								;; If it match with "*", then check next character for "**" token.
								(if (equal res 4)
									(if (and (< i len) (string= (subseq word i (+ i 1)) "*")) (progn (setq i (+ i 1)) (setq res 3)))
								)
								;; If it match with "\"", then check wheter it is OP_OC or OP_CP
								(if (equal res 7) (progn (setq res (+ res (mod opoc 2))) (setq opoc (+ opoc 1))))
								;; tokens: "(" ")" "\"" "," "'"   After these tokens, Any token can come.
								(if (or (equal res 5) (equal res 6) (equal res 7) (equal res 9) (equal res 10))
									(progn (setq tokens (append tokens (list subword))) (setq tokensline (append tokensline (list (nth res OP)))) (setq j i) (setq check 1))
									;; After other tokens, Only possible (defined above) tokens can come.
									(if (>= i len)
										(progn (setq tokens (append tokens (list subword))) (setq tokensline (append tokensline (list (nth res OP)))) (setq check 1))
										(progn
										 	(setq temp (subseq word i (+ i 1)))
										 	(if (equal (findinList temp Possible) nil)
										 		(progn (setq check -1))
										 		(progn (setq tokens (append tokens (list subword))) (setq tokensline (append tokensline (list (nth res OP)))) (setq j i) (setq check 1))
										 	)
										)
									)	
								)
							)	
						)
					)	
				)

				;; Check wheter subword is keyword or not.
				(if (= check 0)
					(progn
						(setq res (findinList subword KeyWord))
						(if (not (equal res nil))
							(if (>= i len)
								(progn (setq tokens (append tokens (list subword))) (setq tokensline (append tokensline (list (nth res KW)))) (setq check 1))
								(progn
								 	(setq temp (subseq word i (+ i 1)))
								 	;; After these keywords, Only possible (defined above) tokens can come.
								 	(if (and (equal (findinList temp Possible) nil))
								 		(if (equal (isID (concatenate 'string subword temp)) nil) 
								 			(progn (setq check -1))
								 		)
								 		(progn (setq tokens (append tokens (list subword))) (setq tokensline (append tokensline (list (nth res KW)))) (setq j i) (setq check 1))
								 	)
								)
							)
						)
					)	
				)

				;; Check wheter subword is value or not.
				;; [0]|[1-9][0-9]*
				(if (= check 0)
					(progn
						(setq res (isVal subword))
						(if (not (equal res nil))
							(progn
								(loop
									(setq temp (string-downcase (subseq word j i)))
									(setq i (+ i 1))
									(when (or (equal (isVal temp) nil) (> i len)) (return))
								)
								(setq i (- i 1))
								(if (equal (isVal temp) nil) (setq i (- i 1)))								
								(if (>= i len)
									(progn (setq tokens (append tokens (list subword))) (setq tokensline (append tokensline (list "VALUE"))) (setq check 1))
									(progn
									 	(setq temp (subseq word i (+ i 1)))
									 	(if (equal (findinList temp Possible) nil)
									 		(progn (setq check -1))
									 		(progn (setq tokens (append tokens (list subword))) (setq tokensline (append tokensline (list "VALUE"))) (setq j i) (setq check 1))
									 	)
									)
								)
							)	
						)
					)
				)

				;; Check wheter subword is comment or not.
				(if (and (= check 0) (string= subword Comment))
						(if (and (< i len) (string= (subseq word i (+ i 1)) Comment))
							(progn (setq tokens (append tokens (list "COMMENT"))) (setq tokensline (append tokensline (list "COMMENT"))) (setq j i) (setq check 2))
						)
				)

				;; Check whether subword is identifier or not.
				(if (= check 0)
					(progn
						(setq res (isID subword))
						(if (equal res t)
							(if (= i len)
								(progn (setq tokens (append tokens (list subword))) (setq tokensline (append tokensline (list "IDENTIFIER")))  (setq check 1))
								(progn
									(setq temp (string-downcase (subseq word j (+ i 1))))
									(setq id (isID temp))
									(if (equal res id)
										()
										(progn
										 	(setq temp (subseq word i (+ i 1)))
										 	(if (equal (findinList temp Possible) nil)
										 		(progn (setq check -1))
										 		(progn (setq tokens (append tokens (list subword))) (setq tokensline (append tokensline (list "IDENTIFIER"))) (setq j i) (setq check 1))
										 	)
										)
									)
								)
							)
							(progn (setq check -1))
						)
					)	
				)
				(if (= check 2) (return check))

			)
		)
		check			
	)
)

;; Split string with any character
(defun split-str (string &optional (separator " "))
  (split-1 string separator))

(defun split-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
      (cons string r))))

;; find a string in list
(defun findinList (word complist &optional (i 0))
	(if (null complist)
		nil
		(if (string= word (car complist))
			i
			(findinList word (cdr complist) (+ i 1))
		)
	)
)

;; is subword a identifier
(defun isID (word)
	(let ((len (- (length word) 1)) (chr) (res t))

		(loop for i from 0 to len
			do
			(progn
				(setq chr (char word i))
				(if (= i 0)
					(if (or (alpha-char-p chr) (char= chr #\_) (char= chr #\.) (char= chr #\+)) (setq res t) (setq res nil))
					(if (or (alpha-char-p chr) (digit-char-p chr) (char= chr #\_) (char= chr #\.) (char= chr #\+)) () (setq res nil))
				)
				(if (equal res nil) (return res))
			)
		)
		res
	)
)

;; is subword a value
(defun isVal (word)
	(let ((chr) (res t))
		(if (equal (every #'digit-char-p word) nil)
			(setq res nil)
			(progn
				(if (= (length word) 1)
					(setq res t)
					(progn
						(setq chr (char word 0))
						(if (equal (digit-char-p chr) 0) (setq res nil) (setq res t))
					)
				)		
			)
		)
		res	
	)
)

(defun evaluate (&optional(ttokens tokens) (ttokensline tokensline)(flag2 0))
	(let ((len (list-length ttokens)) (res 0) (flag 0) (val1 0) (val2 0) (val3 0) (temp 2) (temp2) (val4) (kw) (temp3) (temp4))
		
		(if (and (string= (nth 0 ttokensline) "OP_OP") (string= (nth (- len 1) ttokensline) "OP_CP" ))
			(progn
				(setq kw (nth 1 ttokensline))
				(if (or (string= kw "OP_PLUS") (string= kw "OP_MINUS") (string= kw "OP_MULT") (string= kw "OP_DIV") (string= kw "OP_DBLMULT"))
					(progn
						(setq flag 1)
						(if (or (string= (nth 2 ttokensline) "VALUE") (string= (nth 2 ttokensline) "IDENTIFIER"))
							(progn
								(if (string= (nth 2 ttokensline) "VALUE") (progn (setq val1 (parse-integer (nth 2 ttokens))) (setq temp 3)))
								(if (string= (nth 2 ttokensline) "IDENTIFIER") (progn (setq val1 (findid (nth 2 ttokens))) (setq temp 3)))
							)
							(progn
								(setq temp4 (findexpresion (subseq ttokens 2)))
								(if (equal temp4 nil) (progn (setq res nil) (setq temp nil)) (setq temp (+ temp temp4)))
								(if (and (not (equal temp nil)) (< temp len)) (setq val1 (evaluate (subseq ttokens 2 temp) (subseq ttokensline 2 temp) 1)) (setq res nil))
								(if (equal val1 nil) (setq res nil))
							)
						)
						(if (equal temp nil) (setq res nil) (setq temp2 (+ temp 2)))
						(if (not (equal res nil))
							(if (or (string= (nth temp ttokensline) "VALUE") (string= (nth temp ttokensline) "IDENTIFIER"))
								(progn
									(if (string= (nth temp ttokensline) "VALUE") (progn (setq val2 (parse-integer (nth temp ttokens)))))
									(if (string= (nth temp ttokensline) "IDENTIFIER") (progn (setq val2 (findid (nth temp ttokens)))))
								)
								(progn
									(setq temp4 (findexpresion (subseq ttokens temp)))
									(if (equal temp4 nil) (progn (setq res nil) (setq temp2 nil)) (setq temp2 (+ temp temp4)))
									(if (and (not (equal temp2 nil)) (< temp2 len)) (progn (setq val2 (evaluate (subseq ttokens temp temp2) (subseq ttokensline temp temp2) 1)) (setq temp2 (+ temp2 1))) (setq res nil))
									(if (equal val2 nil) (setq res nil))
								)
							)
						)
						(if (and (not (equal res nil)) (equal temp2 len) (not (equal val1 nil)) (not (equal val2 nil)))
							(progn
								(if (string= kw "OP_PLUS") (setq res (+ val1 val2)))
								(if (string= kw "OP_MINUS") (setq res (- val1 val2)))
								(if (string= kw "OP_MULT") (setq res (* val1 val2)))
								(if (string= kw "OP_DIV") (setq res (/ val1 val2)))
								(if (string= kw "OP_DBLMULT") (setq res (expt val1 val2)))
							)
							(setq res nil)
						)

					)
				)
				(if (string= kw "IDENTIFIER")
					(progn
						(setq flag 1)
						(setq val1 (explisti (subseq ttokens 2 (- len 1)) (subseq ttokensline 2 (- len 1)) 1))
						(if (equal val1 nil) (setq val1 (evaluate (subseq ttokens 2 (- len 1)) (subseq ttokensline 2 (- len 1)) 1)))
						(if (equal val1 nil) (setq res nil) (setq res (nth 1 ttokens)))
						(if (equal flag2 1) (setq res 0))
					)
				)
				(if (and (string= kw "KW_SET") (string= (nth 2 ttokensline) "IDENTIFIER"))
					(progn
						(setq flag 1)
						(setq val1 (explisti (subseq ttokens 3 (- len 1)) (subseq ttokensline 3 (- len 1)) 1))
						(if (equal val1 nil) (setq val1 (evaluate (subseq ttokens 3 (- len 1)) (subseq ttokensline 3 (- len 1)) 1)))
						(if (equal val1 nil)
							(setq res nil)
							(progn
								(setq val2 (position (nth 2 ttokens) symbol :test #'string=))
								(if (equal val2 nil)
									(progn
										(setq symbol (append symbol (list (nth 2 ttokens))))
										(setq symbolval (append symbolval (list val1)))
									)
									(setf (elt symbolval val2) val1)
								)
								(setq res val1)
							)
						)
					)
				)
				(if (string= kw "KW_IF")
					(progn
						(setq flag 1)
						(setq temp3 (nth 2 ttokensline))
						(if (or (string= temp3 "VALUE") (string= temp3 "KW_TRUE") (string= temp3 "KW_FALSE") (string= temp3 "IDENTIFIER"))
							(progn (setq val1 (expb (list (nth 2 ttokens))(list (nth 2 ttokensline)) 1)) (setq temp 3))
							(progn
								(setq temp4 (findexpresion (subseq ttokens 2)))
								(if (equal temp4 nil) (progn (setq res nil) (setq temp nil)) (setq temp (+ temp temp4)))
								(if (not (equal temp nil)) (setq val1 (expb (subseq ttokens 2 temp) (subseq ttokensline 2 temp) 1)) (setq res nil))
							)
						)
						(if (equal temp nil) (setq res nil) (setq temp2 (+ temp 2)))
						(if (not (equal res nil))
							(if (or (string= (nth temp ttokensline) "VALUE") (string= (nth temp ttokensline) "IDENTIFIER"))
								(progn
									(if (string= (nth temp ttokensline) "VALUE") (progn (setq val2 (parse-integer (nth temp ttokens)))))
									(if (string= (nth temp ttokensline) "IDENTIFIER") (progn (setq val2 (findid (nth temp ttokens)))))
								)
								(progn
									(setq temp4 (findexpresion (subseq ttokens temp)))
									(if (equal temp4 nil) (progn (setq res nil) (setq temp2 nil)) (setq temp2 (+ temp temp4)))
									(if (not (equal temp2 nil)) (progn (setq val2 (evaluate (subseq ttokens temp temp2) (subseq ttokensline temp temp2) 1)) (setq temp2 (+ temp2 1))) (setq res nil))
									(if (equal val2 nil) (setq res nil))
								)
							)
						)
						(if (and (not (equal temp2 nil)) (< temp2 len))
							(progn (setq val3 (evaluate (subseq ttokens (- temp2 1) (- len 1)) (subseq ttokensline (- temp2 1) (- len 1)) 1))
							(if (equal val3 nil) (setq res nil) (setq temp2 len)))
							(setq res nil)
						)
						(if (and (not (equal res nil)) (= temp2 len) (not (equal val2 nil)) (not (equal val3 nil)))
							(progn
								(if (equal val1 -2) (setq val1 nil))
								(if (equal val3 nil)
									(if val1 (setq res val2))
									(if val1 (setq res val2) (setq res val3))
								)
							)
							(setq res nil)
						)
					)
				)

				(if (and (string= kw "KW_IF") (equal res nil))
					(progn
						(setq flag 1)
						(setq res 0)
						(setq temp 2)
						(setq temp3 (nth 2 ttokensline))
						(if (or (string= temp3 "VALUE") (string= temp3 "KW_TRUE") (string= temp3 "KW_FALSE") (string= temp3 "IDENTIFIER"))
							(progn (setq val1 (expb (list (nth 2 ttokens))(list (nth 2 ttokensline)) 1)) (setq temp 3))
							(progn
								(setq temp4 (findexpresion (subseq ttokens 2)))
								(if (equal temp4 nil) (progn (setq res nil) (setq temp nil)) (setq temp (+ temp temp4)))
								(if (not (equal temp nil)) (setq val1 (expb (subseq ttokens 2 temp) (subseq ttokensline 2 temp) 1)) (setq res nil))
							)
						)
						(if (equal temp nil) (setq res nil) (setq temp2 (+ temp 2)))
						(if (not (equal res nil))
							(if (string= (nth (- temp 1) ttokensline) "OP_OP")
								(progn
									(setq temp4 (findexpresion (subseq ttokens (- temp 1))))
									(if (equal temp4 nil) (progn (setq res nil) (setq temp2 nil)) (setq temp2 (+ temp temp4)))
									(if (and (not (equal temp2 nil)) (< temp2 len)) (progn (setq val2 (explisti (subseq ttokens temp temp2) (subseq ttokensline temp temp2) 1)) (setq temp2 (+ temp2 1))) (setq res nil))
								)
								(setq res nil)
							)
						)
						(if (not (equal temp2 len))
							(progn (setq val3 (explisti (subseq ttokens (- temp2 1) (- len 1)) (subseq ttokensline (- temp2 1) (- len 1)) 1))
								(if (equal val3 nil) (setq res nil) (setq temp2 len)))
						)
						(if (and (not (equal res nil)) (= temp2 len) (not (equal val1 nil)) (not (equal val2 nil)))
							(progn
								(if (equal val1 -2) (setq val1 nil))
								(if (equal val3 nil)
									(if val1 (setq res val2))
									(if val1 (setq res val2) (setq res val3))
								)
							)
							(setq res nil)
						)
					)

				)
				(if (and (string= kw "KW_FOR") (string= (nth 2 ttokensline) "OP_OP") (string= (nth 3 ttokensline) "IDENTIFIER"))
					(progn
						(setq flag 1)
						(setq val1 (evaluate (list (nth 3 ttokens))(list (nth 3 ttokensline)) 1))
						(if (equal val1 nil) (setq res nil))
						(setq temp 4)
						(if (not (equal val1 nil))
							(if (or (string= (nth temp ttokensline) "VALUE") (string= (nth temp ttokensline) "IDENTIFIER"))
								(progn
									(if (string= (nth temp ttokensline) "VALUE") (progn (setq val2 (parse-integer (nth temp ttokens))) (Setq temp2 5)))
									(if (string= (nth temp ttokensline) "IDENTIFIER") (progn (setq val2 (findid (nth temp ttokens))) (Setq temp2 5)))
								)
								(progn
									(setq temp4 (findexpresion (subseq ttokens temp)))
									(if (equal temp4 nil) (progn (setq res nil) (setq temp2 nil)) (setq temp2 (+ temp temp4)))
									(if (and (not (equal temp2 nil)) (< temp2 len)) (setq val2 (evaluate (subseq ttokens temp temp2) (subseq ttokensline temp temp2) 1)) (setq res nil))
									(if (equal val2 nil) (setq res nil))
								)
							)
						)
						(if (not (equal res nil))
							(if (or (string= (nth temp2 ttokensline) "VALUE") (string= (nth temp2 ttokensline) "IDENTIFIER"))
								(progn
									(if (string= (nth temp2 ttokensline) "VALUE") (progn (setq val3 (parse-integer (nth temp2 ttokens))) (Setq temp3 6)))
									(if (string= (nth temp2 ttokensline) "IDENTIFIER") (progn (setq val3 (findid (nth temp2 ttokens))) (Setq temp3 6)))
								)
								(progn
									(setq temp4 (findexpresion (subseq ttokens temp2)))
									(if (equal temp4 nil) (progn (setq res nil) (setq temp3 nil)) (setq temp3 (+ temp2 temp4)))
									(if (and (not (equal temp3 nil)) (< temp2 len)) (setq val3 (evaluate (subseq ttokens temp2 temp3) (subseq ttokensline temp2 temp3) 1)) (setq res nil))
									(if (equal val3 nil) (setq res nil))
								)
							)
						)
						(if (equal (nth temp3 ttokensline) "OP_CP") (setq temp3 (+ temp3 1)) (setq res nil))
						(if (not (equal res nil))
							(if (string= (nth temp3 ttokensline) "OP_OP")
								(progn
									(setq temp4 (findexpresion (subseq ttokens temp3)))
									(if (equal temp4 nil) (progn (setq res nil) (setq temp4 nil)) (setq temp4 (+ temp3 temp4)))
									(if (and (not (equal temp4 nil)) (< temp4 len)) (progn (setq val4 (explisti (subseq ttokens temp3 temp4) (subseq ttokensline temp3 temp4) 1)) (setq temp4 (+ temp4 1))) (setq res nil))
								)
								(setq res nil)
							)
						)
						(if (and (not (equal res nil)) (= temp4 len) (not (equal val2 nil)) (not (equal val3 nil)) (not (equal val4 nil)))
							(setq res val4)
							(setq res nil)
						)
					)
				)

				(if (and (string= kw "KW_LOAD") (string= (nth 2 ttokensline) "OP_OC") (string= (nth 3 ttokensline) "IDENTIFIER"))
					(progn
						(setq flag 1)
						(setq temp (nth 3 ttokens))
						(setq temp2 (open temp :if-does-not-exist nil))
						(if (equal temp2 nil) (write NIL)(write T)) (terpri)
						(setq res temp)
					)
				)

				(if (and (string= kw "KW_DEFFUN") (string= (nth 2 ttokensline) "IDENTIFIER"))
					(progn
						(setq flag 1)
						(setq temp 3)
						(setq symbol (append symbol (list (nth 2 ttokens))))
						(setq symbolval (append symbolval (list 0)))
						(if (string= (nth 3 ttokensline) "IDENTIFIER")
							(progn (setq val1 0) (setq temp 4))
							(progn
								(setq temp4 (findexpresion (subseq ttokens 3)))
								(if (equal temp4 nil) (progn (setq res nil) (setq temp nil)) (setq temp (+ temp temp4)))
								(if (and (not (equal temp nil)) (< temp len)) (setq val1 (isidlist (subseq ttokens 3 temp) (subseq ttokensline 3 temp))) (setq res nil))
								(if (equal val1 nil) (setq res nil))
							)
						)
						(if (not (equal res nil))
							(if (or (string= (nth temp ttokensline) "VALUE") (string= (nth temp ttokensline) "IDENTIFIER"))
								(progn
									(if (string= (nth temp ttokensline) "VALUE") (progn (setq val2 (parse-integer (nth temp ttokens)))))
									(if (string= (nth temp ttokensline) "IDENTIFIER") (progn (setq val2 (findid (nth temp ttokens)))))
								)
								(progn
									(setq temp4 (findexpresion (subseq ttokens temp)))
									(if (equal temp4 nil) (progn (setq res nil) (setq temp2 nil)) (setq temp2 (+ temp temp4)))
									(if (and (not (equal temp2 nil)) (< temp2 len)) (progn (setq val2 (evaluate (subseq ttokens temp temp2) (subseq ttokensline temp temp2) 1)) (setq temp2 (+ temp2 1))) (setq res nil))
									(if (equal val2 nil) (setq res nil))
								)
							)
						)
						(if (and (not (equal res nil)) (= temp2 len) (not (equal val1 nil)) (not (equal val2 nil)))
							(setq res (nth 2 ttokens))
							(setq res nil)
						)
					)
				)

				(if (and (equal res nil) (string= kw "KW_DEFFUN") (string= (nth 2 ttokensline) "IDENTIFIER"))
					(progn
						(setq flag 1)
						(setq temp 3)
						(if (string= (nth 3 ttokensline) "IDENTIFIER")
							(progn (setq val1 0) (setq temp 4))
							(progn
								(setq temp4 (findexpresion (subseq ttokens 3)))
								(if (equal temp4 nil) (progn (setq res nil) (setq temp nil)) (setq temp (+ temp temp4)))
								(if (and (not (equal temp nil)) (< temp len)) (setq val1 (isidlist (subseq ttokens 3 temp) (subseq ttokensline 3 temp))) (setq res nil))
								(if (equal val1 nil) (setq res nil))
							)
						)
						(if (string= (nth temp ttokensline) "OP_OP")
								(progn
									(setq temp4 (findexpresion (subseq ttokens temp)))
									(if (equal temp4 nil) (progn (setq res nil) (setq temp2 nil)) (setq temp2 (+ temp temp4)))
									(if (and (not (equal temp2 nil)) (< temp2 len)) (progn (setq val2 (explisti (subseq ttokens temp temp2) (subseq ttokensline temp temp2) 1)) (setq temp2 (+ temp2 1))) (setq res nil))
								)
								(setq res nil)
						)
						(if (and (not (equal res nil)) (= temp2 len) (not (equal val1 nil)) (not (equal val2 nil)))
							(setq res (nth 2 ttokens))
							(setq res nil)
						)
					)
				)

				(if (and (string= kw "KW_EXIT") (equal len 3))
					(progn (setq flag 1) (write "Bye.")(terpri) (exit))
				)

				(if (string= kw "KW_DISP")
					(progn
						(setq flag 1)
						(setq val1 (evaluate (subseq ttokens 2 (- len 1)) (subseq ttokensline 2 (- len 1)) 1))
						(if (equal val1 nil)
							(setq val1 (explisti (subseq ttokens 2 (- len 1)) (subseq ttokensline 2 (- len 1)) 1))
						)
						(if (not (equal val1 nil))
							(progn (write val1)(terpri) (setq res val1))
							(setq res nil)
						)
					)
				)

				(if (equal flag 0)
					(progn
						(setq res (expb ttokens ttokensline 0))
					)
				)

			)
			(progn
				(if (string= (nth 0 tokens) "COMMENT")
					(setq flag2 1)
					(progn
						(setq temp3 (nth 0 ttokensline))
						(if (equal len 1)
							(progn
								(if (string= temp3 "VALUE") (setq val1 (parse-integer (nth 0 ttokens))) (setq temp 3))
								(if (string= temp3 "IDENTIFIER") (setq val1 (findid (nth 0 ttokens))) (setq temp 3))
								(setq res val1)
							)
							(setq res nil)
						)
					)
				)
			)
		)
		(if (and (not (equal res nil)) (not (equal res -555)) (= flag2 0)) (write res))
		res
	)
)

(defun expb (&optional(ttokens tokens) (ttokensline tokensline)(flag2 0))
	(let ((len (list-length ttokens)) (res 0) (flag 0) (val1 0) (val2 0) (temp 2) (temp2) (kw) (temp3)(temp4) (flag3 0))
		
		(if (and (string= (nth 0 ttokensline) "OP_OP") (string= (nth (- len 1) ttokensline) "OP_CP" ))
			(progn
				(setq kw (nth 1 ttokensline))
				(if (or (string= kw "KW_AND") (string= kw "KW_OR") (string= kw "KW_EQUAL") (string= kw "KW_LESS"))
					(progn
						(setq flag 1)
						(setq temp3 (nth 2 ttokensline))
						(if (or (string= temp3 "VALUE") (string= temp3 "KW_TRUE") (string= temp3 "KW_FALSE") (string= temp3 "IDENTIFIER"))
							(progn
								(if (string= temp3 "VALUE") (setq val1 (parse-integer (nth 2 ttokens))) (setq temp 3))
								(if (string= temp3 "KW_TRUE") (setq val1 t) (setq temp 3))
								(if (string= temp3 "KW_FALSE") (setq val1 -2) (setq temp 3))
								(if (string= temp3 "IDENTIFIER") (setq val1 (findid (nth 2 ttokens))) (setq temp 3))	
							)
							(progn
								(setq temp4 (findexpresion (subseq ttokens 2)))
								(if (equal temp4 nil) (progn (setq res nil) (setq temp nil)) (setq temp (+ temp temp4)))
								(if (and (not (equal temp nil)) (< temp len)) (progn (setq val1 (expb (subseq ttokens 2 temp) (subseq ttokensline 2 temp) 1)) (setq temp2 (+ temp 2))) (setq res nil))
							)
						)
						(if (equal temp nil) (setq res nil) (setq temp3 (nth temp ttokensline)))
						(if (not (equal res nil))
							(if (or (string= temp3 "VALUE") (string= temp3 "KW_TRUE") (string= temp3 "KW_FALSE") (string= temp3 "IDENTIFIER"))
								(progn
									(if (string= temp3 "VALUE") (setq val2 (parse-integer (nth temp ttokens))))
									(if (string= temp3 "KW_TRUE") (setq val2 t))
									(if (string= temp3 "KW_FALSE") (setq val2 -2))
									(if (string= temp3 "IDENTIFIER") (setq val2 (findid (nth temp ttokens))))
									(setq temp2 5)	
								)
								(progn
									(setq temp4 (findexpresion (subseq ttokens temp)))
									(if (equal temp4 nil) (progn (setq res nil) (setq temp2 nil)) (setq temp2 (+ temp temp4)))
									(if (and (not (equal temp2 nil)) (< temp2 len)) (progn (setq val2 (expb (subseq ttokens temp temp2) (subseq ttokensline temp temp2) 1)) (setq temp2 (+ temp2 1))) (setq res nil))
								)
							)
						)
						(if (and (not (equal res nil)) (equal temp2 len) (not (equal val1 nil)) (not (equal val2 nil)))
							(progn
								(if (equal val1 -2) (setq val1 nil))
								(if (equal val2 -2) (setq val2 nil))
								(if (string= kw "KW_AND") (setq res (and val1 val2)))
								(if (string= kw "KW_OR") (setq res (or val1 val2)))
								(if (string= kw "KW_EQUAL") (setq res (equal val1 val2)))
								(if (string= kw "KW_LESS") (setq res (< val1 val2)))
								(if (= flag2 0) (setq flag3 2)
									(progn (if (equal res nil) (setq res -2))))
							)
							(setq res nil)
						)

					)
				)
				(if (string= kw "KW_NOT")
					(progn
						(setq flag 1)
						(setq temp3 (nth 2 ttokensline))
						(if (or (string= temp3 "VALUE") (string= temp3 "KW_TRUE") (string= temp3 "KW_FALSE") (string= temp3 "IDENTIFIER"))
							(progn
								(if (string= temp3 "VALUE") (setq val1 (parse-integer (nth 2 ttokens))) (setq temp 3))
								(if (string= temp3 "KW_TRUE") (setq val1 t) (setq temp 3))
								(if (string= temp3 "KW_FALSE") (setq val1 nil) (setq temp 3))
								(if (string= temp3 "IDENTIFIER") (setq val1 (findid (nth 2 ttokens))) (setq temp 3))
								(setq res (not val1))
								(if (= flag2 0) (setq flag3 2)
									(progn (if (equal res nil) (setq res -2))))
							)
							(progn
								(setq temp4 (findexpresion (subseq ttokens 2)))
								(if (equal temp4 nil) (progn (setq res nil) (setq temp nil)) (setq temp (+ temp temp4)))
								(if (and (not (equal temp nil)) (< temp len)) (progn (setq val1 (expb (subseq ttokens 2 temp) (subseq ttokensline 2 temp) 1)) (setq temp (+ temp 1))) (setq res nil))
								(if (and (not (equal res nil)) (equal temp len) (not (equal val1 nil)))
									(progn
										(setq res (not val1))
									)
									(setq res nil)
								)
							)
						)
					)
				)

				(if (equal flag 0) (setq res nil))

			)
			(progn
				(setq temp3 (nth 0 ttokensline))
				(if (equal len 1)
					(progn
						(if (string= temp3 "VALUE") (setq val1 (parse-integer (nth 0 ttokens))) (setq temp 3))
						(if (string= temp3 "KW_TRUE") (setq val1 t) (setq temp 3))
						(if (string= temp3 "KW_FALSE") (setq val1 -2) (setq temp 3))
						(if (string= temp3 "IDENTIFIER") (setq val1 (findid (nth 0 ttokens))) (setq temp 3))
						(setq res val1)
					)
					(setq res nil)
				)
			)
		)
		(if (or (and (not (equal res nil)) (= flag2 0)) (= flag3 2)) (write res))
		(if (and (= flag2 0) (= flag3 2)) (setq res -555))
		res
	)
)

(defun explisti (&optional(ttokens tokens) (ttokensline tokensline)(flag2 0))
	(let ((len (list-length ttokens)) (res 0) (flag 0) (val1 0) (val2 0) (temp 2) (temp2) (temp4) (kw))
		
		(if (and (string= (nth 0 ttokensline) "OP_OP") (string= (nth (- len 1) ttokensline) "OP_CP" ))
			(progn
				(setq kw (nth 1 ttokensline))
				(if (and (or (string= kw "KW_APPEND") (string= kw "KW_CONCAT")) (or (string= (nth 2 ttokensline) "CUT") (string= (nth 3 ttokensline) "KW_LIST")))
					(progn
						(setq flag 1)
						(setq temp4 (findexpresion (subseq ttokens 2)))
						(if (equal temp4 nil) (progn (setq res nil) (setq temp nil)) (setq temp (+ temp temp4)))
						(if (equal temp nil) (setq res nil) (setq temp2 (+ temp 2)))
						(if (and (not (equal temp nil)) (< temp len)) (setq val1 (explisti (subseq ttokens 2 temp) (subseq ttokensline 2 temp) 1)) (setq res nil))
						(if (and (not (equal temp nil)) (< temp len))
							(progn
								(setq temp4 (findexpresion (subseq ttokens temp)))
								(if (equal temp4 nil) (progn (setq res nil) (setq temp2 nil)) (setq temp2 (+ temp temp4)))
							)
							(setq temp2 nil))
						(if (and (not (equal temp2 nil)) (< temp2 len)) (progn (setq val2 (explisti (subseq ttokens temp temp2) (subseq ttokensline temp temp2) 1)) (setq temp2 (+ temp2 1))) (setq res nil))
						(if (and (not (equal res nil)) (= temp2 len) (not (equal val1 nil)) (not (equal val2 nil)))
							(progn
								(setq res (list))
								(setq res (append res val1))
								(setq res (append res val2))
							)
							(setq res nil)
						)

					)
				)
				(if (and (equal res nil) (string= kw "KW_APPEND"))
					(progn
						(setq flag 1)
						(if (or (string= (nth 2 ttokensline) "VALUE") (string= (nth 2 ttokensline) "IDENTIFIER"))
							(progn
								(if (string= (nth 2 ttokensline) "VALUE") (progn (setq val1 (parse-integer (nth 2 ttokens))) (Setq temp 3)))
								(if (string= (nth 2 ttokensline) "IDENTIFIER") (progn (setq val1 (findid (nth 2 ttokens))) (Setq temp 3)))
							)
							(if (string= (nth 2 ttokensline) "IDENTIFIER")
								(progn (setq val1 (list (findid (nth 2 ttokens)))) (setq temp 3))
								(progn
									(setq temp4 (findexpresion (subseq ttokens 2)))
									(if (equal temp4 nil) (progn (setq res nil) (setq temp nil)) (setq temp (+ temp temp4)))
									(if (not (equal temp nil)) (setq val1 (evaluate (subseq ttokens 2 temp) (subseq ttokensline 2 temp) 1)) (setq res nil))
									(if (equal val1 nil) (setq res nil))
								)
							)
						)
						(if (equal temp nil) (setq res nil) (setq temp2 (+ temp 2)))

						(if (and (not (equal temp nil)) (< temp len))
							(progn
								(setq temp4 (findexpresion (subseq ttokens temp)))
								(if (equal temp4 nil) (progn (setq res nil) (setq temp2 nil)) (setq temp2 (+ temp temp4)))
							)
							(setq temp2 nil))
						(if (and (not (equal temp2 nil)) (< temp2 len)) (progn (setq val2 (explisti (subseq ttokens temp temp2) (subseq ttokensline temp temp2) 1)) (setq temp2 (+ temp2 1))) (setq res nil))
							
						(if (and (not (equal res nil)) (= temp2 len) (not (equal val1 nil)) (not (equal val2 nil)))
							(progn
								(setq res (list))
								(setq res (append res val1))
								(setq res (append res val2))
							)
							(setq res nil)
						)
					)
				)

				(if (string= (nth 1 ttokensline) "KW_LIST")
					(progn
						(setq flag 1)
						(setf (elt ttokens 1) "(")
						(setf (elt ttokensline 1) "OP_OP")
						(setq val1 (convlist (subseq ttokens 1) (subseq ttokensline 1)))
						(if (equal val1 nil) (setq res nil) (setq res val1))
					)
				)
				(if (equal flag 0) (setq res nil))	

			)
			(progn
				(if (string= (nth 0 ttokensline) "CUT") (setq res (convlist (subseq ttokens 1) (subseq ttokensline 1))) (setq res nil))
				(setq res nil)
			)
		)
		(if (and (not (equal res nil)) (= flag2 0)) (write res))
		res
	)
)

(defun convlist (ttokens ttokensline)
	(let ((len (list-length ttokens)) (kw) (res 1) (val1) (llist (list)))
		(if (and (string= (nth 0 ttokensline) "OP_OP") (string= (nth (- len 1) ttokensline) "OP_CP" ) (> len 2))
			(progn
				(loop for i from 1 to (- len 2)
				 do(progn
				 		(setq kw (nth i ttokensline))
				 		(if (string= kw "VALUE")
				 			(setq val1 (list (parse-integer (nth i ttokens))))
				 			(if (string= kw "IDENTIFIER")  (setq val1 (findid (nth i ttokens))) (setq res nil))
				 		)
				 		(if (not (equal res nil)) (setq llist (append llist val1)))
				 	)
				)
				(if (not (equal res nil)) (setq res llist))
			)
			(setq res nil)
		)
		res
	)
)

(defun findid (exp)
	(let((res 0))
		(setq res (position exp symbol :test #'string=))
		(if (equal res nil)
			(progn (format t "variable ~S has no value." exp) (terpri) (exit))
			(setq res (nth res symbolval))
		)
		res
	)
)

(defun isidlist (ttokens ttokensline)
	(let ((len (list-length ttokensline)) (kw) (res 1))
		(if (and (string= (nth 0 ttokensline) "OP_OP") (string= (nth (- len 1) ttokensline) "OP_CP" ) (> len 2))
			(progn
				(loop for i from 1 to (- len 2)
				 do(progn
				 		(setq kw (nth i ttokensline))
				 		(if (string= kw "IDENTIFIER")  (progn (setq symbol (append symbol (list (nth i ttokens)))) (setq symbolval (append symbolval (list 0)))) (setq res nil))
				 	)
				)
			)
			(setq res nil)
		)
		res
	)
)

(defun findexpresion (exp)
	(let ((counter 0) (str) (j 0) (res nil))
		(if (string= (nth 0 exp) "(")
			(progn
				(loop for i in exp
					do (progn
						(setq str (string-downcase i))
						(if (string= str "(") (setq counter (+ counter 1)))
						(if (string= str ")") (setq counter (- counter 1)))
						(setq j (+ j 1))
						(if (= counter 0) (return j))
					)
				)
				(setq res j)
			)
		)
		res
	)
)

(defvar filename)

;; Determine run format (gppinterpreter) or (gppinterpreter filename)
(defun runformat ()
	(let ((rline) (res))
		(setq rline (read-line))
		(terpri)
		(setq rline (string-trim '(#\Space #\Tab #\Newline) rline))
		(if (string= rline "(gppinterpreter)")
			(setq res 1)
			(progn
				(setq rline (split-str rline "\"" ))
				(if (and (= (list-length rline) 3) (string= "(gppinterpreter " (nth 0 rline)) (string= ")" (nth 2 rline)))   
					(progn (setq filename (nth 1 rline)) (setq res 2))
					(setq res nil)
				)
			)
		)
		res
	)
)

(defvar run (runformat))
(if (= run 1) (gppinterpreter))
(if (= run 2) (gppinterpreter filename))
(if (= run -1) (write "G++ Starting method is incorrect."))