(defvar copl-trans-regexp ">>\\|evalto\\|--->\\|-*->\\|-d->\\||-\\|->\\|=>\\|==>")

(defvar copl-keyword '("by" "def"))

(defvar copl-rule
  '("P-Zero" "P-Succ" "T-Zero" "T-Succ"
    "L-Succ" "L-Trans" "L-Zero" "L-SuccSucc"
    "L-SuccR" "E-Const" "E-Plus" "E-Times"
    "R-Plus" "R-Times" "R-PlusL" "R-PlusR"
    "R-TimesL" "R-TimesR" "DR-Plus" "DR-Times"
    "DR-PlusL" "DR-PlusR" "DR-TimesL" "DR-TimesR"
    "MR-Zero" "MR-Multi" "MR-One" "E-Int"
    "E-Bool" "E-IfT" "E-IfF" "E-Plus" "E-Minus"
    "E-Times" "E-Lt" "B-Plus" "B-Minus" "B-Times"
    "B-Lt" "E-PlusBoolL" "E-PlusBoolR" "E-PlusErrorL"
    "E-PlusErrorR" "E-MinusBoolL" "E-MinusBoolR" "E-MinusErrorL"
    "E-MinusErrorR" "E-TimesBoolL" "E-TimesBoolR" "E-TimesErrorL"
    "E-TimesErrorR" "E-LtBoolL" "E-LtBoolR" "E-LtErrorL"
    "E-LtErrorR" "E-IfInt" "E-IfError" "E-IfTError" "E-IfFError"
    "E-Var1" "E-Var2" "E-Let" "E-Fun" "E-App" "E-LetRec"
    "E-AppRec" "Tr-Int" "Tr-Bool" "Tr-If" "Tr-Plus" "Tr-Minus"
    "Tr-Times" "Tr-Lt" "Tr-Var1" "Tr-Var2" "Tr-Let"
    "Tr-Fun" "Tr-App" "Tr-LetRec" "E-Var" "E-Nil"
    "E-Cons" "E-MatchNil" "E-MatchCons" "M-Var"
    "M-Nil" "M-Cons" "M-Wild" "NM-ConsNil" "NM-NilCons"
    "NM-ConsConsL" "NM-ConsConsR" "E-MatchM1" "E-MatchM2"
    "E-MatchN" "T-Int" "T-Bool" "T-If" "T-Plus"
    "T-Minus" "T-Times" "T-Lt" "T-Var" "T-Let"
    "T-Fun" "T-App" "T-LetRec" "T-Nil" "T-Cons"
    "T-Match" "T-Abs" "E-Int" "E-Bool"
    "E-BinOp" "E-If" "C-Ret" "C-EvalR" "C-Plus"
    "C-Minus" "C-Times" "C-Lt" "C-IfT" "C-IfF"
    "E-LetCc" "C-LetBody" "C-EvarArg" "C-EvalFun"
    "C-EvalFunR" "C-EvalFunC" "C-EvalConsR" "C-Cons"
    "C-MatchNil" "C-MatchCons" "E-Ref" "E-Deref" "E-Assign" ))

(defvar copl-keyword-regexp (regexp-opt copl-keyword 'words))
(defvar copl-rule-regexp (regexp-opt copl-rule 'words))

(setq copl-keyword nil)
(setq copl-rule nil)

(defun copl-comment-dwim (arg)
  "Comment and uncommend curren line or region in a smart way."
  (interactive "*P")
  (require 'newcomment)
  (let ((comment-start "//") (comment-end ""))
    (comment-dwim arg)))

(setq copl-font-lock-keywords
      `(
        (,copl-trans-regexp . font-lock-function-name-face)
        (,copl-keyword-regexp . font-lock-keyword-face)
        (,copl-rule-regexp . font-lock-constant-face)
))

(defvar copl-tab-width 4)


(define-derived-mode copl-mode fundamental-mode
  "copl mode"
  "Major mode for CoPL"

  (setq font-lock-defaults '((copl-font-lock-keywords)))

  (set (make-local-variable 'indent-tab-mode) nil)
  (set (make-local-variable 'tab-width) copl-tab-width)
  (set (make-local-variable 'indent-line-function) 'copl-indent-line)

  ;;modify keymap
  (define-key copl-mode-map [remap comment-dwim] 'copl-comment-dwim)

  ;; comment
  (modify-syntax-entry ?\/ ". 12b" copl-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" copl-mode-syntax-table)

  (modify-syntax-entry ?\( ". 1" copl-mode-syntax-table)
  (modify-syntax-entry ?\) ". 4" copl-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23" copl-mode-syntax-table)

  ;;indentation
  (set (make-local-variable 'indent-line-function) 'copl-indent-line)

  ;; clear memory
  (setq copl-keyword-regexp nil)
  (setq copl-trans-regexp nil)
  (setq copl-rule-regexp nil)
)


;; indentation
(defun count-char (str c)
  (let ((len (length str))
        (i 0)
        (ret 0))
    (while (< i len)
      (when (char-equal (aref str i) c)
        (incf ret))
      (incf i))
    ret))

(defun count-level ()
  (- (count-char-inbuf ?{ (point-min) (point)) (count-char-inbuf ?} (point-min) (+ (point) 1))))

(defun count-char-inbuf (c start end)
    (count-char (buffer-substring start end) c))

(defun delete-spaces ()
  (while (char-equal (aref (buffer-substring (point) (+ (point) 1)) 0) 32)
    (delete-char 1)))

(defun copl-indent-line ()
  (let ((l (count-level)))
    (delete-spaces)
    (insert-char 32 (* 4 l))
    (when (string= (buffer-substring (point) (+ (point) 1)) "}")
      (forward-char 1))))

(provide 'copl-mode)
