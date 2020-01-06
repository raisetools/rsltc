;******************************************
;******************************************

(defvar best-match)
(defvar best-length)
(defvar point-save)
(defvar rest-of-recogniser)
(defvar match-action)

(defun got-here (mtoken bmatch) nil)

(defun match-length () (- (match-end 0)(match-beginning 0)))

(defun match-token()
      (goto-char (1- point-save))
      (g-c-t best-length))

(defun g-c-t (length)
  (if (= length 0)
    ""
    (progn
     (forward-char 1)
     (concat (char-to-string (following-char)) (g-c-t (1- length)))
    )))

(defun find-maximal-token (strings)
   (progn 
      (setq point-save (point))
      (setq best-length 0)
      (setq rest-of-recogniser strings)
      (while (not (null rest-of-recogniser))
          (progn
             (goto-char point-save)
             (if (re-search-forward (car (car rest-of-recogniser)) (mark) t)
                 (if (and (> (match-length) best-length)
                          (= (match-beginning 0) point-save))
                    (progn (setq best-match (car rest-of-recogniser))
                           (setq best-length (match-length))
                    )))
             (setq rest-of-recogniser (cdr rest-of-recogniser))
         ))
      (goto-char point-save)
      (re-search-forward (car best-match))
      (goto-char point-save)
      (setq match-action (car (cdr best-match)))
))

(defun select (strings)
   (if (vectorp strings)
     (elt strings (following-char))
     strings))

(defun tokenise (strings) 
    (progn
       (find-maximal-token (select strings))
       (got-here (match-token) best-match)
       (replace-match (if (stringp match-action) 
                         match-action
                         (apply match-action ()))
       t)
     ))


(defun not-finished ()
   (progn
     (setq point-save (point))
     (< (point) (mark))))


(defun tokenise-region ( strings inter-token init-tok term-tok )
    (progn
      (setq case-fold-search nil)
      (apply init-tok ())
      (while (not-finished)
           (progn (tokenise strings) (apply inter-token())))
      (apply term-tok())
    ))



;**********************************************
;**********************************************


;     simple optimiser - looks at first character of the string
;     if it is specific, adds to the list specifically for that
;     character otherwise adds to the lists for all characters.

(defun optimise (strings) "" (interactive)
    (optimise-to-vector (reverse strings) (make-vector 128 nil)))

(defvar str-opt)
(defvar vect-opt)
(defun optimise-to-vector (strings vector)
    (progn
       (setq vect-opt vector)
       (setq str-opt strings)
       (while (not (null str-opt))
         (progn
          (add-to-vector (car str-opt) (elt (car (car str-opt)) 0) 1)
          (setq str-opt (cdr str-opt))
         ))
       vect-opt))

(defvar opt-count)
(defun add-to-vector (pair elem next-elt)
    (cond
      ( (= elem (string-to-char "\\"))
              (cond
                ((= (elt (car pair) next-elt) (string-to-char "b")) (add-to-all pair))
                ((= (elt (car pair) next-elt) (string-to-char "B")) (add-to-all pair))
                ((= (elt (car pair) next-elt) (string-to-char "w")) (add-to-all pair))
                ((= (elt (car pair) next-elt) (string-to-char "W")) (add-to-all pair))
                ((= (elt (car pair) next-elt) (string-to-char "(")) (add-to-all pair))
                ((= (elt (car pair) next-elt) (string-to-char ")")) (add-to-all pair))
                ((= (elt (car pair) next-elt) (string-to-char "|")) (add-to-all pair))
                ((= (elt (car pair) next-elt) (string-to-char "s")) (add-to-all pair))
                ((= (elt (car pair) next-elt) (string-to-char "S")) (add-to-all pair))
                ((= (elt (car pair) next-elt) (string-to-char ">")) (add-to-all pair))
                ((= (elt (car pair) next-elt) (string-to-char "<")) (add-to-all pair))
                (t (aset vect-opt (elt (car pair) next-elt) 
                         (cons pair (elt vect-opt (elt (car pair) next-elt)))))))

      ( (= elem (string-to-char "[")) (add-to-all pair))
      ( (= elem (string-to-char ".")) (add-to-all pair))
      ( (= elem (string-to-char "^")) (if (= 1 next-elt)
         (add-to-vector pair (elt (car pair) next-elt) 1)
         (aset vect-opt elem (cons pair (elt vect-opt elem)))))
      ( t (aset vect-opt elem (cons pair (elt vect-opt elem))))
    ))



(defun add-to-all (val)
   (progn
     (setq opt-count 0)
     (while (< opt-count 128)
      (progn
       (aset vect-opt opt-count (cons pair (elt vect-opt opt-count)))
       (setq opt-count (1+ opt-count))))
))
