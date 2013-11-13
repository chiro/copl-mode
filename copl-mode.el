(defvar copl-trans-regexp ">>\\|evalto\\|--->\\|-*->\\|-d->\\||-\\|->\\|=>\\|==>")

(defvar copl-keyword '("by" "def"))
(defvar copl-keyword-regexp (regexp-opt copl-keyword 'words))

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
    "C-MatchNil" "C-MatchCons" "E-Ref" "E-Deref" "E-Assign"))

(defvar copl-rule-regexp (regexp-opt copl-rule 'words))
(setq copl-rule nil)


(defun copl-comment-dwim (arg)
  "Comment and uncommend curren line or region in a smart way."
  (interactive "*P")
  (require 'newcomment)
  (let ((comment-start "//") (comment-end ""))
    (comment-dwim arg)))

(defvar copl-mode-hook nil)

;; copl-mode-map is empty.
(defvar copl-mode-map (make-sparse-keymap))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.copl\\'" . copl-mode))

(defconst copl-font-lock-keywords
  (list
   '(">>\\|evalto\\|--->\\|-*->\\|-d->\\||-\\|->\\|=>\\|==>" . font-lock-function-name-face)
   '("\\<\\(by\\|def\\)\\>" . font-lock-keyword-face)
   '("\\<\\(B-\\(?:Lt\\|\\(?:Minu\\|Plu\\|Time\\)s\\)\\|C-\\(?:Cons\\|Eva\\(?:l\\(?:ConsR\\|Fun[CR]?\\|R\\)\\|rArg\\)\\|If[FT]\\|L\\(?:etBody\\|t\\)\\|M\\(?:atch\\(?:Cons\\|Nil\\)\\|inus\\)\\|Plus\\|Ret\\|Times\\)\\|DR-\\(?:Plus[LR]?\\|Times[LR]?\\)\\|E-\\(?:A\\(?:pp\\(?:Rec\\)?\\|ssign\\)\\|B\\(?:inOp\\|ool\\)\\|Const?\\|Deref\\|Fun\\|I\\(?:f\\(?:Error\\|FError\\|Int\\|TError\\|[FT]\\)?\\|nt\\)\\|L\\(?:et\\(?:\\(?:C\\|Re\\)c\\)?\\|t\\(?:Bool[LR]\\|Error[LR]\\)?\\)\\|M\\(?:atch\\(?:Cons\\|M[12]\\|N\\(?:il\\)?\\)\\|inus\\(?:Bool[LR]\\|Error[LR]\\)?\\)\\|Nil\\|Plus\\(?:Bool[LR]\\|Error[LR]\\)?\\|Ref\\|Times\\(?:Bool[LR]\\|Error[LR]\\)?\\|Var[12]?\\)\\|L-\\(?:Succ\\(?:R\\|Succ\\)?\\|Trans\\|Zero\\)\\|M\\(?:-\\(?:Cons\\|Nil\\|Var\\|Wild\\)\\|R-\\(?:Multi\\|One\\|Zero\\)\\)\\|NM-\\(?:Cons\\(?:Cons[LR]\\|Nil\\)\\|NilCons\\)\\|P-\\(?:Succ\\|Zero\\)\\|R-\\(?:Plus[LR]?\\|Times[LR]?\\)\\|T\\(?:-\\(?:A\\(?:bs\\|pp\\)\\|Bool\\|Cons\\|Fun\\|I\\(?:f\\|nt\\)\\|L\\(?:et\\(?:Rec\\)?\\|t\\)\\|M\\(?:atch\\|inus\\)\\|Nil\\|Plus\\|Succ\\|Times\\|Var\\|Zero\\)\\|r-\\(?:App\\|Bool\\|Fun\\|I\\(?:f\\|nt\\)\\|L\\(?:et\\(?:Rec\\)?\\|t\\)\\|Minus\\|Plus\\|Times\\|Var[12]\\)\\)\\)\\>" . font-lock-constant-face))
  "Default highlighting expressions for CoPL mode")

(defvar copl-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?\/ ". 12b" st)
    (modify-syntax-entry ?\n "> b" st)

    (modify-syntax-entry ?\( ". 1" st)
    (modify-syntax-entry ?\) ". 4" st)
    (modify-syntax-entry ?*  ". 23" st)
    st)
  "Syntax table for copl-mode")


(defun copl-mode ()
  "Major mode for CoPL"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table copl-mode-syntax-table)
  (use-local-map copl-mode-map)
  (set (make-local-variable 'font-lock-defaults)
       '(copl-font-lock-keywords))

  (setq major-mode 'copl-mode)
  (setq mode-name "CoPL")
  (run-hooks 'copl-mode-hook))

(provide 'copl-mode)
