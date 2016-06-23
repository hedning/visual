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
     (let* ((range (,func ,back))
            (beg (plist-get range :beg))
            (end (plist-get range :end)))
       (goto-char (if ,back beg end))
       (if ,shift
           (plist-put range (if ,back :end :beg)
                      (if ,back (plist-get visual--selection :end)
                        (plist-get visual--selection :beg))))
       (setq visual--selection range)
       (visual-overlay-range range))))

(defun visual-select-overlay ()
  (interactive)
  (goto-char (plist-get visual--selection :beg))
  (set-mark (plist-get visual--selection :end)))

(defun visual-exit-state ()
  (interactive)
  (hlt-unhighlight-region))

(add-hook 'evil-lisp-state-exit-hook 'visual-exit-state)

;; (defun visual-next ()
;;   (interactive)
;;   )

;; (defun visual-previous ()
;;   (interactive)
;;   )

;; (defun visual-up ()
;;   (interactive)
;;   )

;; (defun visual-down ()
;;   (interactive)
;;   (sp-get-enclosing-sexp)
;;   )

(make-sp-visual visual-forward-symbol sp-get-symbol nil)
(make-sp-visual visual-backward-symbol sp-get-symbol 1)
(make-sp-visual visual-forward-sexp sp-get-thing nil)
(make-sp-visual visual-backward-sexp sp-get-thing t)
(make-sp-visual visual-up sp-get-enclosing-sexp 1)
(make-sp-visual visual-down sp-get-sexp nil)

(defun visual-down ()
  (interactive)
  (let* ((sexp (plist-get (sp-get-sexp) :beg))
         (range)
         (beg)
         (end))
    (save-excursion
      (when (equal (plist-get visual--selection :op) "(")
        ()
        (goto-char (plist-get visual--selection :beg)))
      (setq range (sp-get-symbol)
            beg (plist-get range :beg)
            end (plist-get range :end)))
    (when range
      (goto-char end)
      (setq visual--selection range)
      (hlt-unhighlight-region)
      (hlt-highlight-region beg end 'region))
    range))

(defun visual-up-or-backwards ()
  (interactive)
  )

(let ((bindings
       '(("l" . visual-forward-sexp)
         ("h" . visual-backward-sexp)
         ("k" . visual-up)
         ("j" . visual-down)
         ("v" . visual-select-overlay))))
  (dolist (binding bindings)
    (let ((key (car binding))
          (cmd (cdr binding)))
      (message "cmd: %s" cmd)
      (eval `(progn
              (define-key evil-lisp-state-map ,(kbd key)
                (evil-lisp-state-enter-command ,cmd)))))))

;; (define-key evil-lisp-state-map "l" 'visual-forward-sexp)
;; (define-key evil-lisp-state-map "h" 'visual-backward-sexp)
;; (define-key evil-lisp-state-map "L" 'visual-forward-symbol)
;; (define-key evil-lisp-state-map "H" 'visual-backward-symbol)
;; (define-key evil-lisp-state-map "v" 'visual-select-overlay)
;; (define-key evil-lisp-state-map "k" 'visual-up)
;; (define-key evil-lisp-state-map "j" 'visual-down)

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
