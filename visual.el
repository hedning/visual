(defvar visual--current-type nil
  "The current semantic unit that we're selecting")

(defun visual-select-range (thing)
  (interactive)
  (message "thing: %s" thing)
  (when thing
    (let ((beg (plist-get thing :beg))
          (end (plist-get thing :end)))
      (message "beg: %s" beg)
      (message "end: %s" end)
      ;; (if (evil-visual-state-p)
      ;;   (goto-char (1- beg)))
      (goto-char beg)
      (set-mark end))))

(defun visual--direction (&optional back)
  (if (< (evil-visual-direction) 0)
      ;; put point at the start (end) of the selection
      (unless back
        (exchange-point-and-mark))
    (when back
      (exchange-point-and-mark))))

;; ;; (defun visual-select-symbol
;; ;;     )

;; (defun visual-select-forward-thing (&optional back)
;;   (interactive)
;;   (visual--direction back)
;;   (visual-select-range (sp-get-thing back)))

;; (defun visual-select-forward-symbol (&optional back)
;;   (interactive)
;;   (visual--direction back)
;;   (visual-select-range (sp-get-symbol back)))

;; (defun visual-select-backward-symbol (&optional forward)
;;   (interactive)
;;   (if forward
;;       (visual-select-forward-symbol)
;;     (visual-select-forward-symbol t)))

;; (spacemacs/set-leader-keys "os" 'visual-select-forward-symbol)

;; (evil-define-state viSual
;;   "viSual state. Move your selections around"
;;   :tag " <VS> "
;;   :enable (motion)
;;   (cond ((evil-viSual-state-p)
;;          (visual-select-forward-symbol)
;;          )
;;         (t
;;          (deactivate-mark))))

;; (defun visual-insert (&optional end)
;;   (interactive)
;;   (if (< (mark) (point))
;;       (unless end
;;         (exchange-point-and-mark))
;;     (when end
;;       (exchange-point-and-mark)))
;;   (evil-change-to-previous-state))

;; (defface spacemacs-viSual-face '((t ())) "viSual face")

;; (define-key evil-visual-state-map "v" nil)
;; (evil-define-key 'visual evil-visual-state-map
;;   "i" 'visual-insert
;;   "w" 'visual-select-forward-symbol
;;   "b" 'visual-select-backward-symbol
;;   (kbd "ESC") 'evil-change-to-previous-state)

;; (define-key evil-visual-state-map "w" 'visual-select-forward-symbol)
;; (define-key evil-visual-state-map "b" 'visual-select-backward-symbol)
;; (define-key evil-visual-state-map "v" 'er/expand-region)


;; (defun visual-highlight-thing (thing)
;;   (interactive)
;;   (message "thing: %s" thing)
;;   (when thing
;;     (let ((beg (sp-get thing :beg))
;;           (end (sp-get thing :end)))
;;       (message "beg: %s" beg)
;;       (message "end: %s" end)
;;       ;; (if (evil-visual-state-p)
;;       ;;   (goto-char (1- beg)))
;;       (hlt-unhighlight-region)
;;       (hlt-highlight-region beg end))))

;; (defun visual-forward-sexp (&optional arg)
;;   (interactive)
;;   (let* ((thing (sp-forward-sexp arg))
;;          (beg (sp-get thing :beg))
;;          (end (sp-get thing :end)))
;;     (hlt-unhighlight-region)
;;     (hlt-highlight-region beg end 'region)))

;; (defun visual-backward-symbol (&optional arg))

(defvar visual--selection nil
  "")
(make-variable-buffer-local 'visual--selection)

(defun visual-overlay-range (range)
  (when range
    (let ((beg (plist-get range :beg))
          (end (plist-get range :end)))
      (hlt-highlight-region beg end 'region)
      range)))

(defmacro make-sp-visual (name func back shift)
  `(defun ,name ()
     (interactive)
     (let ((continue (memq (point) visual--selection)))
       (let* ((range (,func ,back))
              (beg (plist-get range :beg))
              (end (plist-get range :end)))
         (goto-char (if ,back beg end))
         (if (and ,shift continue)
             (plist-put range (if ,back :end :beg)
                        (if ,back
                            (plist-get visual--selection :end)
                          (plist-get visual--selection :beg))))
         (setq visual--selection range)
         (visual-overlay-range range)))))

(defun visual-select-overlay ()
  (interactive)
  (goto-char (plist-get visual--selection :beg))
  (set-mark (plist-get visual--selection :end)))

(defun visual-exchange-end-and-beg ()
  (interactive)
  (when (memq (point) visual--selection)
    (let ((beg (plist-get visual--selection :beg))
          (end (plist-get visual--selection :end)))
      (if (eq (point) beg)
          (goto-char end)
        (goto-char beg))
      (visual-overlay-range visual--selection))))

(defun visual-exit-state ()
  (interactive)
  (hlt-unhighlight-region))

(add-hook 'evil-lisp-state-exit-hook 'visual-exit-state)



(make-sp-visual visual-forward-symbol sp-get-symbol nil nil)
(make-sp-visual visual-backward-symbol sp-get-symbol 1 nil)
(make-sp-visual visual-forward-sexp sp-get-thing nil nil)
(make-sp-visual visual-backward-sexp sp-get-thing t nil)
(make-sp-visual visual-forward-shift-sexp sp-get-thing nil t)
(make-sp-visual visual-backward-shift-sexp sp-get-thing t t)
(make-sp-visual visual-up sp-get-enclosing-sexp 1 nil)

(defun visual-down (&optional back)
  (interactive)
  (let (range beg end)
    (save-excursion
      (when (equal (plist-get visual--selection :op) "(")
        (goto-char (plist-get visual--selection (if back :end :beg)))
        (if back (backward-char) (forward-char)))
      (setq range (sp-get-thing back)
            beg (plist-get range :beg)
            end (plist-get range :end)))
    (when range
      (goto-char (if back beg end))
      (setq visual--selection range)
      (hlt-unhighlight-region)
      (hlt-highlight-region beg end 'region))
    range))

(defun visual-down-end ()
  (interactive)
  (visual-down t))

(defun visual-up-or-backwards ()
  (interactive)
  )

(let ((bindings
       '(("l" . visual-forward-sexp)
         ("h" . visual-backward-sexp)
         ("k" . visual-up)
         ("j" . visual-down)
         ("J" . visual-down-end)
         ("H" . visual-backward-shift-sexp)
         ("L" . visual-forward-shift-sexp)
         ("v" . visual-select-overlay))))
  (dolist (x bindings)
    (let ((key (car x))
          (cmd (cdr x)))
      (message "cmd: %s" cmd)
      (eval `(progn
              (define-key evil-lisp-state-map ,(kbd key)
                (evil-lisp-state-enter-command ,cmd)))))))

(define-key evil-lisp-state-map (kbd dotspacemacs-leader-key) spacemacs-default-map)
(define-key evil-lisp-state-map (kbd dotspacemacs-major-mode-leader-key)
  (lambda () (interactive) (symbol-value (intern (format "spacemacs-%s-map" major-mode)))))

;; (define-key evil-lisp-state-map (kbd "SPC") 'visual-select-overlay)

(defun select-symbol ()
    (interactive)
    (visual-select-range (sp-get-symbol)))

(define-key evil-motion-state-map "v" 'select-symbol)

;;
;; left right, move through the children 
;; move up and down the hiearchy
