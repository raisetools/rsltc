(load "tokenise")
(setq max-lisp-eval-depth 2000)

;******************************************
;******************************************
(defvar in-comment)
(defvar in-line-comment)
(defvar comment-depth)
(defvar inner-layout)
(defvar tabbing-on)
(defvar counter)
(defvar in-batch nil)

(defconst commenting-recogniser
 '((".*\n" "%\\&")
))

(defconst layout-recogniser (optimise
  '(
     ("`alpha'*" greek-escape)
     ("`beta'*" greek-escape)
     ("`gamma'*" greek-escape)
     ("`Gamma'*" greek-escape)
     ("`delta'*" greek-escape)
     ("`Delta'*" greek-escape)
     ("`epsilon'*" greek-escape)
     ("`zeta'*" greek-escape)
     ("`eta'*" greek-escape)
     ("`theta'*" greek-escape)
     ("`Theta'*" greek-escape)
     ("`iota'*" greek-escape)
     ("`kappa'*" greek-escape)
     ("`Lambda'*" greek-escape)
     ("`mu'*" greek-escape)
     ("`nu'*" greek-escape)
     ("`xi'*" greek-escape)
     ("`Xi'*" greek-escape)
     ("`pi'*" greek-escape)
     ("`Pi'*" greek-escape)
     ("`rho'*" greek-escape)
     ("`sigma'*" greek-escape)
     ("`Sigma'*" greek-escape)
     ("`tau'*" greek-escape)
     ("`upsilon'*" greek-escape)
     ("`Upsilon'*" greek-escape)
     ("`phi'*" greek-escape)
     ("`Phi'*" greek-escape)
     ("`chi'*" greek-escape)
     ("`psi'*" greek-escape)
     ("`Psi'*" greek-escape)
     ("`omega'*" greek-escape)
     ("`Omega'*" greek-escape)
     ("/\\*"        enter-comment)
     ("*\\*/"       star-leave-comment)
     ("*/"          leave-comment)
     ("|-"          "{\\\\TURNSTILE}")
     ("><"          "{\\\\TIMES}")
     ("~="          "{\\\\NOTEQ}")
     ("\\+\\+"      "{\\\\TIE}")
     ("*"           "{\\\\AST}")
     ("*\\*"        make-symbol_up-arrow)
     ("+"           "{\\\\PLUS}")
     ("-"           "{\\\\MINUS}")
     ("->"          "{\\\\RIGHTARROW}")
     ("-m->"        "{\\\\MARROW}")
     ("<->"         "{\\\\lrarrow}")
     ("-set"        "\\\\kw{-set}")
     ("-list"       "$^{\\\\ast}$")
     ("-infset"     "\\\\kw{-infset}")
     ("-inflist"    "$^{\\\\omega}$")
     ("-~->"        "{\\\\PARRIGHTARROW}")
     ("-~m->"       "{\\\\PARMARROW}")
     ("<:"          enter-place-holder )
     ("<"           "{\\\\LT}" )
     ("<="          "{\\\\LEQ}" )
     ("="           "{\\\\EQ}")
     ("=>"          "{\\\\DBLRIGHTARROW}")
     (":>"          leave-place-holder )
     (">"           "{\\\\GT}" )
     (">="          "{\\\\GEQ}")
     ("<<"          "{\\\\SUBSET}")
     (">>"          "{\\\\SUPSET}")
     ("<<="         "{\\\\SUBSETEQ}")
     (">>="         "{\\\\SUPSETEQ}")
     ("\\\\"        "{\\\\SETMINUS}")
     ("\\^"         "{\\\\CONCAT}")
     ("\\[="        "{\\\\SQSUBSETEQ}")
     ("{="          "{\\\\IMPLEMENTS}")
     ("{"           enter-set)
     ("|"           "{\\\\BAR}")
     ("}"           leave-set  )
     ("("           enter-bra )
     (")"           leave-bra )
     ("<\\."        enter-list )
     ("\\.>"        leave-list )
     ("\\["         enter-sq )
     ("\\]"         leave-sq )
     ("{\\."        enter-italics )
     ("\\.}"        leave-italics )
     ("\\[\\."      enter-semantics )
     ("\\.\\]"      leave-semantics )
     ("(:"          enter-goal)
     (":)"          leave-goal)
     ("||"          "{\\\\PARL}")
     ("|\\^|"       "{\\\\NONDETCHOICE}")
     ("|=|"         "{\\\\DETCHOICE}")
     ("\\[\\]"      "{\\\\emptymap}")
     ("~"           "{\\\\SIM}")
     (":-"          "{\\\\RDOT}")
     ("\\.\\.\\."   "{\\\\DOTDOTDOT}")
     ("\\.\\."      "{\\\\DOTDOT}")
     ("#"           "{\\\\HASH}")
     ("!!"          make-symbol_dagger)
     ("\\+>"        "{\\\\MAPSTO}")
     ("_"           "{\\\\UNDERLINE}")
     ("/\\\\"       "{\\\\WEDGE}")
     ("\\\\/"       "{\\\\VEE}")
     ("-\\\\"       "{\\\\LAMBDA}")
     ("all"         make-symbol_all)
     ("always"      make-symbol_always)
     ("inter"       make-symbol_inter)
     ("exists"      make-symbol_exists)
     ("is"          make-symbol_is)
; for method book
;     ("<is>"        make-symbol_ruleequiv)
     ("isin"        "{\\\\ISIN}")
     ("~isin"       "{\\\\NOTISIN}")
     ("union"       make-symbol_union)
     ("Bool"        make-keyword)
     ("Char"        make-keyword)
     ("Int"         make-keyword)
     ("Nat"         make-keyword)
     ("Real"        make-keyword)
     ("Text"        make-keyword)
     ("Unit"        make-keyword)
     ("abs"         make-keyword)
     ("any"         make-keyword)
     ("as"          make-keyword)
     ("axiom"       make-keyword)
     ("card"        make-keyword)
     ("case"        enter-structured-expr)
; for method book
;     ("cases"       enter-structured-expr)
     ("channel"     make-keyword)
     ("chaos"       make-keyword)
     ("class"       enter-structured-expr)
     ("devt_relation"   make-keyword)
; addition for method book
;     ("development_relation"   make-keyword)
     ("do"          enter-structured-expr)
     ("dom"         make-keyword)
     ("elems"       make-keyword)
     ("else"        make-keyword)
     ("elsif"       make-keyword)
     ("end"         leave-structured-expr)
     ("extend"      make-keyword)
     ("false"       make-keyword)
; for method book
;     ("follows"     enter-structured-expr)
     ("for"         make-keyword)
; for method book
;     ("forall"      make-keyword)
; for method book
;     ("from"        make-keyword)
     ("hd"          make-keyword)
     ("hide"        make-keyword)
     ("if"          enter-structured-expr)
     ("in"          make-keyword)
     ("inds"        make-keyword)
     ("initialise"  make-keyword)
     ("int"         make-keyword)
; for method book
;     ("irrelevant"  make-keyword)
; for method book
;     ("justification"   enter-structured-expr)
; for method book
;     ("lemma"       enter-structured-expr)
     ("len"         make-keyword)
     ("let"         enter-structured-expr)
     ("local"       enter-structured-expr)
     ("object"      make-keyword)
     ("of"          make-keyword)
     ("out"         make-keyword)
; for method book
;     ("properties"  make-keyword)
     ("post"        make-keyword)
     ("pre"         make-keyword)
; for method book
;     ("qed"         make-keyword)
     ("read"        make-keyword)
     ("real"        make-keyword)
     ("rng"         make-keyword)
     ("scheme"      make-keyword)
     ("skip"        make-keyword)
; for method book
;     ("since"       enter-structured-expr)
     ("stop"        make-keyword)
     ("swap"        make-keyword)
     ("then"        make-keyword)
     ("theory"      enter-structured-expr)
     ("tl"          make-keyword)
     ("true"        make-keyword)
     ("type"        make-keyword)
     ("until"       make-keyword)
     ("use"         make-keyword)
     ("value"       make-keyword)
     ("variable"    make-keyword)
; for method book
;     ("when"        make-keyword)
     ("while"       make-keyword)
     ("with"        make-keyword)
     ("write"       make-keyword)
; keywords for proof rules
;     ("are_different" make-keyword)
;     ("assignment_disjoint" make-keyword)
;     ("convergent"  make-keyword)
;     ("disjoint"    make-keyword)
;     ("express"     make-keyword)
;     ("is_literal"  make-keyword)
;     ("is_maximal"  make-keyword)
;     ("isin_subtype" make-keyword)
;     ("matches"     make-keyword)
;     ("no_capture"  make-keyword)
;     ("no_hiding"   make-keyword)
;     ("no_new_capture" make-keyword)
;     ("no_scheme_defs" make-keyword)
;     ("pure"        make-keyword)
;     ("qualify"     make-keyword)
;     ("readonly"    make-keyword)
;     ("readwriteonly" make-keyword)
;     ("subst_binding" make-keyword)
;     ("subst_expr"  make-keyword)
; test_case added
      ("test_case"   make-keyword)

     ("--\\([^\n]*\\)" line-comment) ; line comment

;    leading optional ~ is to catch identifiers starting ~isin
     ("[~]?[a-zA-Z_][a-zA-Z_0-9']*" escape-chars)

;     ("\\.\\([0-9],[0-9]\\)" "$_{\\1}$")
;     ("\\.\\([0-9]\\)" "$_{\\1}$")

     ("[0-9]+" "\\&")
     ("[0-9]+\\.[0-9]+" "\\&")

     ("\"\\(\\([^\\\"\n]\\|\\\\.\\)*\\)\""  "{\\\\PRIM}{\\\\PRIM}\\\\verb&\\1&{\\\\PRIM}{\\\\PRIM}")
     ("\""          unterminated-string)

     ("'\\(\\\\?.\\)'"  "{\\\\PRIM}\\\\verb&\\1&{\\\\PRIM}")
     ("'\\(\\\\[0-7]\\{1,3\\}\\)'" "{\\\\PRIM}\\\\verb&\\1&{\\\\PRIM}") ; octal
     ("'\\(\\\\x[0-9a-fA-F]\\{1,2\\}\\)'" "{\\\\PRIM}\\\\verb&\\1&{\\\\PRIM}") ; hex
     ("'\\\\?.[^']"      incomplete-character)
     ("`"           "{\\\\BQUOT}")

     ("\\\\[a-zA-Z]+{[^}]*}" "\\&") ; LaTeX commands with one argument
     ("\\\\[a-zA-Z---]+{"        enter-latex-command)

     ("{!\\(.*\\)!}"      "{\\\\LBRACE}!\\1!{\\\\RBRACE}") ; error messages

     ("\n"          rsl-line)
     ("^[ ]+"       tabs)
     ("[ ]+"        sps)

     ("&&"          "&")
     ("&\\([^&\n]+\\)&"  "\\1")
     ("&[^&\n]*"    unmatched-ampersand)

     ("." "\\&")        ; catch-all
)))

(defconst decommenting-recogniser
 '( ("^%\\(.*\n\\)" "\\1")
    ("\n%\\(.*\n\\)" "\\1")  ; bug in regexp-match can consider a
                             ; single new line as the start of the next
    ("^[^%\n].*\n" "")       ; 
    ("\n" "")                ;
  ))

(defconst tabstop-recogniser
    '((" "   " ")
      ("[ ]+" place-tab)
      ("."   "\\&")
      ("\n"   new-tabbing-line)))

;********************************************************
;********************************************************

(defun display ()
     (sit-for 0))

(defun init-layout ()
  (progn 
      (setq in-comment nil)
      (setq comment-depth 0)
      (setq in-line-comment nil)
      (init-bra)))

(defun term-layout ()
  (check-clear))

(defun nothing ()
      nil)


;******************************************************
;******************************************************

(defvar col)
(defvar tabstops)
(defvar kill-string)
(defvar tabline-assoc)
(defvar tab-assoc)
(defvar next-logical-tab)
(defvar next-logical-space)
(defvar this-line-number)
(defvar current-tab)

(defun make-kill-string () (interactive)
  (progn
    (setq tab-assoc nil)
    (setq current-tab 1)
    (setq col 0)
    (setq kill-string "")
    (setq tabstops (sort tabstops '<))
    (while (not (null tabstops))
       (progn
          (setq kill-string 
             (if (= col (car tabstops))
                 (progn
                     (setq tab-assoc (cons
                                        (cons col (cons current-tab nil))
                                        tab-assoc))
                     (setq current-tab (1+ current-tab))
                     (concat kill-string "\\=M"))
                 (concat kill-string "M")))
          (while (and (not (null tabstops)) (= (car tabstops ) col))
              (setq tabstops (cdr tabstops)))
          (setq col (1+ col))
       ))
    (setq kill-string (concat kill-string "\\kill\n"))))

(defun set-tab ()
  (progn
   (setq tabstops
         (cons (1+ (current-column) ) tabstops))
   (setq tabline-assoc 
      (cons (list  (+ (* 20 this-line-number) next-logical-space)
                   (1+ (current-column)))
            tabline-assoc))
   (setq next-logical-space (1+ next-logical-space))))

(defun restart-tabs ()
   (progn (setq next-logical-tab 0) (setq next-logical-space 0)))

(defun get-tabs ()
  (let* ((col (car (cdr (assoc (+ (* 20 this-line-number) next-logical-space) tabline-assoc))))
         (this-tab (car (cdr (assoc col tab-assoc))))
         (mt (build-string (- this-tab next-logical-tab) "\\\\>")))

            (setq next-logical-space (1+ next-logical-space))
            (setq next-logical-tab this-tab)
             mt))



;******************************************************
;******************************************************

(defvar suffix-subscript nil
  "Causes trailing digits on identifiers not to be subscripts")

(defun rsl-suffix-subscript-on () (interactive)
   (setq suffix-subscript t))
 
(defun rsl-suffix-subscript-off () (interactive)
   (setq suffix-subscript nil))

(defvar checks-on t "Sets the automatic checks for the RSL convertor")

(defun rsl-check-on () (interactive)
   (setq checks-on t))
 
(defun rsl-check-off () (interactive)
   (setq checks-on nil))

(defvar comment-checks-on t "Sets the checks for brackets on in RSL comments")

(defun rsl-comment-check-on () (interactive) (setq comment-checks-on t))

(defun rsl-comment-check-off () (interactive) (setq comment-checks-on nil))

(defun init-bra () (setq bstack '(1000)))
(defun check-clear () (check 1000))

(defun enter-list ()
  (progn
    (enter-bracket 1)
    "{\\\\LANGLE}"))

(defun enter-bra ()
  (progn
    (enter-bracket 2)
    "("))

(defun enter-set ()
  (progn
    (enter-bracket 3)
    "{\\\\LBRACE}"))

(defun enter-sq ()
  (progn
    (enter-bracket 4)
    "{\\\\LBRACKET}"))

(defun enter-italics ()
  (progn
    (enter-bracket 5)
    "{\\\\it "))

(defun enter-semantics ()
  (progn
    (enter-bracket 6)
    "{\\\\leftsem}"))

(defun enter-structured-expr ()
  (progn
    (enter-bracket 8)
    (make-keyword)))

(defun enter-goal ()
  (progn
    (enter-bracket 9)
    "{\\\\LEFTGOALBR}"))

(defun enter-place-holder ()
  (progn
    (enter-bracket 10)
    "\\\\PH{"))

(defun enter-latex-command ()
  (progn
    (enter-bracket 11)
    "\\&"))

(defun enter-bracket (arg)
  (let ((checking nil))
    (if checks-on
	(if in-comment
	    (if (and comment-checks-on (not (= arg 8)))
		(setq checking t))
	  (setq checking t)))
    (if checking
	(setq bstack (cons arg bstack)))
  ))

(defun leave-list ()
  (progn
    (check 1)
    "{\\\\RANGLE}"))

(defun leave-bra ()
  (progn
    (check 2) 
    ")"))

(defun leave-set ()
; check if leaving latex command
  (if (and (not (null bstack)) (= 11 (car bstack)))
      (progn
	(check 11)
	"}")
    (progn
      (check 3)
      "{\\\\RBRACE}")))

(defun leave-sq ()
  (progn
    (check 4)
    "{\\\\RBRACKET}"))

(defun leave-italics ()
  (progn
    (check 5)
    "}"))

(defun leave-semantics ()
  (progn
    (check 6)
    "{\\\\rightsem}"))

(defun leave-structured-expr ()
  (progn
    (check 8)
    (make-keyword)))

(defun leave-goal ()
  (progn
    (check 9)
    "{\\\\RIGHTGOALBR}"))

(defun leave-place-holder ()
  (progn
    (check 10)
    "}"))

(defun check (v1)
  (let ((checking nil))
    (if checks-on
	(if in-comment
	    (if (and comment-checks-on (not (= v1 8)))
		(setq checking t))
	  (setq checking t)))
    (if checking
	(if (and (not (null bstack)) (= v1 (car bstack)))
	    (setq bstack (cdr bstack))
	  (report-error v1)))))

(defun report-message (msg)
  (if in-batch
      (message msg)
    (error msg)))
      

(defun report-error (v1)
(if (= v1 1) (report-message "mis-matched closing list bracket")
(if (= v1 2) (report-message "mis-matched closing parenthesis")
(if (= v1 3) (report-message "mis-matched closing set bracket")
(if (= v1 4) (report-message "mis-matched closing square bracket")
(if (= v1 5) (report-message "mis-matched closing italic bracket")
(if (= v1 6) (report-message "mis-matched closing semantic bracket")
(if (= v1 7) (report-message "mis-matched closing comment bracket")
(if (= v1 8) (report-message "mis-matched `end'")
(if (= v1 9) (report-message "mis-matched closing goal bracket")
(if (= v1 10) (report-message "mis-matched closing place-holder bracket")
(if (= v1 11) (report-message "mis-matched closing latex command bracket")
(while (not (null bstack))
  (progn
    (if (= (car bstack) 1) (report-message "un-matched opening list bracket")
    (if (= (car bstack) 2) (report-message "un-matched opening parenthesis")
    (if (= (car bstack) 3) (report-message "un-matched opening set bracket")
    (if (= (car bstack) 4) (report-message "un-matched opening square bracket")
    (if (= (car bstack) 5) (report-message "un-matched opening italic bracket")
    (if (= (car bstack) 6) (report-message "un-matched opening semantic bracket")
    (if (= (car bstack) 7) (report-message "un-matched opening comment bracket")
    (if (= (car bstack) 8) (report-message "missing `end'")
    (if (= (car bstack) 9) (report-message "un-matched opening goal bracket")
    (if (= (car bstack) 10) (report-message "un-matched opening place-holder bracket")
    (if (= (car bstack) 11) (report-message "un-matched opening latex command bracket")
      nil)))))))))))
    (setq bstack (cdr bstack))))))))))))))))

(defun unterminated-string()
    (if (and checks-on (not in-comment))
	(error "start of unterminated string")
      "\\&"))

(defun incomplete-character()
    (if (and checks-on (not in-comment))
	(error "incomplete character")
      "\\&"))

(defun illegal-char ()
    (if (and checks-on (not in-comment))
	(error "illegal character")
      "\\&"))

(defun unmatched-ampersand ()
    (if (and checks-on (not in-comment))
	(error "unmatched ampersand")
      "\\&"))

(defun line-comment ()
  (setq in-line-comment t)
  (let* ((tok (match-token))
	 (len (match-length))
	 (tok1 (substring tok 2))	; drop leading --
	 (len1 (- len 2))
	 (res (concat "-\\\\,-" (escape-ul tok1 "" 0 len1))))
    (setq in-line-comment nil)
    res))

(defun escape-chars ()
  (if suffix-subscript
   (let ((tok (match-token))
	 (len (match-length)))
     (let ((front-len (suffix-start tok len len)))
       (if (= front-len len)
	 (escape-ul tok "" 0 front-len)
	 (concat (escape-ul tok "" 0 front-len)
		 "$_{"
		 (substring tok front-len nil)
		 "}$"))))
     (escape-ul (match-token) "" 0 (match-length)))) 

(defun suffix-start (tok len endlen)
  (if (numeric (elt tok (1- len)))
    (suffix-start tok (1- len) endlen)
    (if (= (elt tok (1- len)) (string-to-char "_"))
	endlen
      len)))

(defun numeric (char)
  (and (>= char (string-to-char "0")) (<= char (string-to-char "9"))))

(defun escape-ul (tok rettok ind len)
   (if (= ind len)
      rettok
      (escape-ul
        tok
        (concat rettok (conv (elt tok ind)))
        (1+ ind)
        len)))

(defun conv (char)
   (cond
    ((= char (string-to-char "_")) "\\\\_") 
    ((= char (string-to-char "#")) "\\\\#")
    ((= char (string-to-char "'"))
     (if in-line-comment "'" "{\\\\PRIM}"))
    ((= char (string-to-char "~")) "{\\\\SIM}")
    ((= char (string-to-char "\\")) "{\\\\SETMINUS}")
    ((= char (string-to-char "$")) "\\\\$")
    ((= char (string-to-char "%")) "\\\\%")
    ((= char (string-to-char "&")) "\\\\&")
    ((= char (string-to-char "^")) "{\\\\CONCAT}")
    ((= char (string-to-char "{")) "{\\\\LBRACE}")
    ((= char (string-to-char "}")) "{\\\\RBRACE}")
    (t (char-to-string char))))




(defun greek-escape ()
  (let ((tok (match-token))
	(len (match-length)))
    (let ((front-len (1+ (alpha-end tok (1- len)))))
      (concat "$\\\\"
	      (substring tok 1 front-len)
	      "$"
	      (mult-primes (- len front-len))))))

(defun alpha-end (tok pos)
  (if (= (string-to-char "'") (elt tok pos))
      (alpha-end tok (1- pos))
    pos))

(defun mult-primes (num)
  (if (= num 0)
      ""
    (concat "{\\\\PRIM}" (mult-primes (1- num)))))

(defun enter-comment ()
    (progn
      (setq in-comment t)
      (setq comment-depth (1+ comment-depth))
      (setq bstack (cons 7 bstack))
      "{\\\\LCOMMENT}"))

(defun leave-comment ()
    (progn
      (setq comment-depth (1- comment-depth))
      (check 7)
      (setq in-comment (> comment-depth 0))
      "{\\\\RCOMMENT}"))

(defun star-leave-comment ()
  (let ((rcomm (leave-comment)))
    (concat "*" rcomm)))

(defun rsl-line ()
    (progn
       (sit-for 0)
       (setq this-line-number (1+ this-line-number))
       (restart-tabs)
;;       (setq in-comment nil)
       (if (= (mark) (match-end 0)) "\n" "\\\\\\\\\n")))

(defun make-keyword ()
   (if in-comment
      "\\&"
      (concat "\\\\kw{" (escape-chars) "}")))

(defun make-symbol_up-arrow ()
  (if in-comment "**" "{\\\\UPARROW}"))

(defun make-symbol_dagger ()
  (if in-comment "!!" "{\\\\DAGGER}"))

(defun make-symbol_all ()
   (if in-comment
      "\\&"
      "{\\\\ALL}"))

(defun make-symbol_always ()
   (if in-comment
      "\\&"
      "{\\\\ALWAYS}"))

(defun make-symbol_inter ()
   (if in-comment
      "\\&"
      "{\\\\INTER}"))

(defun make-symbol_exists ()
   (if in-comment
      "\\&"
      "{\\\\EXISTS}"))

(defun make-symbol_is ()
   (if in-comment
      "\\&"
      "{\\\\IS}"))

(defun make-symbol_ruleequiv ()
   (if in-comment
      "\\&"
      "{\\\\RULEEQUIV}"))

(defun make-symbol_union ()
   (if in-comment
      "\\&"
      "{\\\\UNION}"))

(defun tabs ()
   (if tabbing-on
      (if (= (match-length) 1)
	 " "
	 (if inner-layout
	    (get-tabs)
	    (concat (build-string (min (/ (match-length) 2) 12) "\\\\>")
		    (if (< (match-length) 24)
		       (if (= (match-length) (* (/ (match-length) 2) 2))
		          ""
		          "\\\\ ")
		       (build-string (- (match-length) 24) "\\\\ ")))))
      (sps)))

(defun sps ()
   (if (= (match-length) 1) 
      " "
      (if inner-layout
         (get-tabs)
         (build-string (match-length) "\\\\ "))))

(defun build-string (len str)
   (if (zerop len) ""
       (concat str (build-string (1- len) str))))

(defun new-tabbing-line ()
    (progn
       (setq this-line-number (1+ this-line-number))
       (restart-tabs)
       "\n"))

(defun place-tab ()
  (progn
   (sit-for 0)
   (set-tab)
   "\\&"))
    
;******************************************************
;******************************************************

(defun flip-point-and-mark ()
  (goto-char (prog1 (mark t)
			  (set-marker (mark-marker) (point)
				      (current-buffer)))))

(defun do-rsl () "" (interactive)
  (progn
    (setq inner-layout nil)
    (setq tabstops nil)
    (setq tab-assoc nil)
    (setq tabline-assoc nil)
    (setq next-logical-space 0)
    (setq next-logical-tab 0)
    (setq this-line-number 0)
    (let ((font-lock-was-on nil))
      ;; turn off font lock if it is on to avoid slowing things down
      (if (and (not in-batch)
	       (boundp 'font-lock-mode)
	       font-lock-mode)
	  (progn
	    (setq font-lock-was-on t)
	    (font-lock-mode -1)))
      (re-search-backward "^\\\\RSLatex")
      (beginning-of-line 1)
      (let ((start (point)))
	(re-search-forward "^\\\\endRSLatex")
	(beginning-of-line 2)
	;; if using version 18 of emacs remove last parameter to push-mark
	(push-mark (point) t t)
	(goto-char start)
	(copy-region-as-kill (point) (mark))
	(insert (current-kill 0))
	;; if using version 18 of emacs remove last parameter to push-mark
	(push-mark (point) t t)
	(goto-char start))
      (untabify (region-beginning) (region-end))
      (tokenise-region commenting-recogniser 'nothing 'nothing 'nothing)
      (kill-line 1)
      (insert "\\bp\n")
      ;; if using version 18 of emacs remove last parameter to push-mark
      (push-mark (point) t t)
      (re-search-forward "^\\\\endRSLatex")
      (beginning-of-line 1)
      (kill-line 1)
      (insert "\\ep\n")
      (forward-line -1)
      (flip-point-and-mark)
      (setq inner-layout nil)
      (untabify (region-beginning) (region-end))
      (setq tabbing-on t)
      (tokenise-region layout-recogniser 'nothing 'init-layout 'term-layout)
      (if font-lock-was-on (font-lock-mode 1))
      (if (not in-batch) (message "do-rsl completed"))
      )))
      
(defun rsl-l () "" (interactive)
    (progn
      (setq inner-layout t)
      (setq tabstops nil)
      (setq tab-assoc nil)
      (setq tabline-assoc nil)
      (setq next-logical-space 0)
      (setq next-logical-tab 0)
      (setq this-line-number 0)   

      (re-search-backward "^/B")
      (forward-line 1)
      (beginning-of-line 1)
      (set-mark (point))
      (re-search-forward "^/E")
      (beginning-of-line 1)
 
      (flip-point-and-mark)
      (untabify (region-beginning) (region-end))
      (tokenise-region tabstop-recogniser 'nothing 'nothing 'nothing)

      (setq this-line-number 0)
      (setq next-logical-tab 0)
      (setq next-logical-space 0)
      (rsl-tabbing-region)))

(defun rsl-tabbing-region ()
      (re-search-backward "^/B")
      (beginning-of-line 1)
      (kill-line 1)
      (insert "\\bp\n")
      (if inner-layout (insert (make-kill-string)) nil)
      (set-mark (point))
      (re-search-forward "^/E")
      (beginning-of-line 1)
      (flip-point-and-mark)
      (copy-region-as-kill (point) (mark))
      (yank)
      (flip-point-and-mark)
      (untabify (region-beginning) (region-end))
      (tokenise-region commenting-recogniser 'nothing 'nothing 'nothing)
;      (insert "\n") Removed to allow \infrule to work SWH 21/10/88


      (set-mark (point))
      (re-search-forward "^/E")
      (beginning-of-line 1)
      (kill-line 1)
      (insert "\\ep\n")
      (forward-line -1)
      (flip-point-and-mark)

      (untabify (region-beginning) (region-end))
      (setq tabbing-on t)
      (tokenise-region layout-recogniser 'display 'init-layout 'term-layout)
    )


(defun rsl-n () "" (interactive)
    (progn
      (setq inner-layout nil)
      (search-backward "/b")
      (delete-char 2)
;; if using version 18 of emacs remove last parameter to push-mark
      (push-mark (point) t t)
      (search-forward "/e")
      (delete-char -2)
      (flip-point-and-mark)
      (rsl-region)
      (message "rsl-n completed")
    ));

(defun rsl-expr ()
  (interactive)
  (let ((end (point)))
    (re-search-backward "[^][0-9a-zA-Z._(){}`']")
    (forward-char 1)
    (insert "\\Rsl{/b")
    (goto-char (+ end 7))
    (insert "/e}")
    (rsl-n)
    (forward-char 1)))

(defun rsl-region () "" (interactive)
  (progn
      (untabify (region-beginning) (region-end))
      (setq tabbing-on nil)
      (tokenise-region layout-recogniser 'nothing 'init-layout 'term-layout)
    ));

(defun undo-rsl () "" (interactive)
   (save-excursion
      (re-search-backward "^%\\\\RSLatex")
      (let ((beg (point)))
         (re-search-forward "^\\\\ep$")
         (beginning-of-line 2)
         (narrow-to-region beg (point)))
      (goto-char (point-min))
      (delete-non-matching-lines "^%")
      (goto-char (point-min))
      (while (re-search-forward "^%" nil t)
	(replace-match "" nil nil))
      (widen)
      (if (not in-batch) (message "undo-rsl completed"))
   ))

(defun redo-rsl () "" (interactive)
   (progn (undo-rsl) (do-rsl) (if (not in-batch) (message "redo-rsl completed"))))

(defun redo-l () "" (interactive)
   (progn (undo-rsl) (rsl-l) (if (not in-batch) (message "redo-t completed"))))

; old compatability functions

(defun rsl-t () "" (interactive) (do-rsl))

(defun rsl-latex () "" (interactive)
   (progn
      (goto-char (min (point) (mark)))
      (setq inner-layout nil)

      (beginning-of-line 1)
      (insert "\n/B\n")
      (flip-point-and-mark)
      (end-of-line 1)
      (insert "\n/E\n")
      (do-rsl)))


;single symbol conversion


(defun real-white-space ()
   (or (= (following-char) 10)
       (= (following-char) 32)))
    
(defun backward-white-space ()
    (or (= (following-char) 10)
       (= (following-char) 32)
       (bobp)))

(defun forward-white-space ()   
   (or (= (following-char) 10)
       (= (following-char) 32)
       (eobp)))

(defvar save-check)

(defun latex-symbol () "" (interactive)
   (progn
      (setq save-check checks-on)
      (rsl-check-off)
      (if (real-white-space) (while (backward-white-space)
                                      (backward-char 1))
                                  nil)

      (while (not (backward-white-space))
         (backward-char 1))
      (if (real-white-space) (forward-char 1) nil)

      (set-mark-command nil)
      (while (not (forward-white-space))
         (forward-char 1))
      (flip-point-and-mark)
      (rsl-region)
      (setq checks-on save-check)
      ))


(defun do-latex-batch () ""
  (progn
    (setq in-batch t)
    (do-latex)
    (setq in-batch nil)))

(defun do-latex () "" (interactive)
    (progn
      (setq counter 0)
      (goto-char (point-min))
      (while (re-search-forward "^\\\\RSLatex" (point-max) t)
          (progn
            (setq counter (+ 1 counter))
            (rsl-t)
	    (end-of-line)
	    (if in-batch
	      (insert "\n%segment " (int-to-string counter))
	      nil)
            (message "%s%d%s" "segment " counter " completed")))
;      (goto-char (point-min))
;      (while (search-forward "/b" (point-max) t)
;          (rsl-n))
      (message "do-latex completed")))

(defun redo-latex ()
  ""
  (interactive)
  (progn
    (undo-latex)
    (do-latex)
    ))

(defun undo-latex ()
  ""
  (interactive)
  (progn
    (goto-char (point-min))
    (while (re-search-forward "^\\\\ep$" (point-max) t)
      (undo-rsl))
    (message "undo-latex completed")))

(defun undo-latex-batch () ""
  (progn
    (setq in-batch t)
    (undo-latex)
    (goto-char (point-min))
    (delete-matching-lines "^%segment"))
    (setq in-batch nil))
  


