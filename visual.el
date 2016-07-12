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

(defvar visual--overlay nil
  "The current overlay used by visual-lisp-state")
(make-variable-buffer-local 'visual--overlay)

(defun visual-make-highlighted-overlay (start end)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'region)
    overlay))

(defun visual-highlight-overlay (overlay)
  (overlay-put overlay 'face 'region)
  overlay)
(defun visual-unhighlight-overlay (overlay)
  (overlay-put overlay 'face nil)
  overlay)

(defun visual-overlay-range (range)
  (when range
    (let ((beg (plist-get range :beg))
          (end (plist-get range :end)))
      (hlt-highlight-region beg end 'region)
      range)))

;; Need to check if the char is equal to any openers
(defun visual-overlay-open ()
  (char-to-string (char-after (overlay-start visual--overlay))))

(defun visual-overlay-close ()
  (char-to-string (char-before (overlay-start visual--overlay))))

(defun visual-at-sexp ()
  (or (equal "(" (visual-overlay-open))
      (equal ")" (visual-overlay-close))))

(defun visual-at-start ()
  (if (eq (point) (overlay-start visual--overlay)) t nil))

(defun visual-at-end ()
  (if (eq (point) (overlay-end visual--overlay)) t nil))

(defun visual-at-overlay ()
  (if (and (evil-lisp-state-p)
           (<= (overlay-start visual--overlay) (point))
           (<= (point) (overlay-end visual--overlay)))))

(defmacro make-sp-visual (name func back shift)
  `(defun ,name ()
     (interactive)
     ;; (evil-lisp-state-p) doesn't work
     ;; because we've already entered evil-lisp-state
     (let* ((old-beg (overlay-start visual--overlay))
            (old-end (overlay-end visual--overlay))
            (at-beg (eq (point) old-beg))
            (at-end (eq (point) old-end))
            (continue (evil-lisp-state-p)))
       (when (and continue (not ,shift))
         (goto-char (if ,back
                        old-beg
                      old-end)))
       (let* ((range (,func ,back)) beg end)
         (when range
           (when (and ,shift continue)
             ;; If needed run the motion again so we know we've found another thing
             (when (or (and at-end ,back)
                       (and at-beg (not ,back)))
               (goto-char (plist-get range (if at-beg :end :beg)))
               (setq range (,func ,back)))
             (setq  beg (plist-get range :beg)
                    end (plist-get range :end))
             (plist-put range :beg (if at-end (min old-beg beg) beg))
             (plist-put range :end (if at-beg (max old-end end) end)))
           (setq beg (plist-get range :beg)
                 end (plist-get range :end))
           (if ,shift
               (goto-char (if at-beg beg end))
             (goto-char (if ,back beg end)))
           (move-overlay visual--overlay beg end))))))

(defun visual-select-overlay ()
  (interactive)
  (goto-char (overlay-start visual--overlay))
  (set-mark (overlay-end visual--overlay)))

(defun visual-goto-start ()
  (when (visual-at-overlay)
    (goto-char (overlay-start visual--overlay))))

(defun visual-goto-end ()
  (when (visual-at-overlay)
    (goto-char (overlay-end visual--overlay))))

(defun visual-exchange-end-and-beg ()
  "Toggle point between stard and end of the current thing"
  (interactive)
  (when (visual-at-overlay)
    (cond ((visual-at-start) (visual-goto-end))
          ((visual-at-end) (visual-goto-start)))))

(defun visual-exit-state ()
  (interactive)
  (visual-unhighlight-overlay visual--overlay)
  (hlt-unhighlight-region))

(defun visual-entry-state ()
  (visual-highlight-overlay visual--overlay))



(add-hook 'evil-lisp-state-exit-hook 'visual-exit-state)
(add-hook 'evil-lisp-state-entry-hook 'visual-entry-state)
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
      (when (equal (visual-overlay-open) "(")
        (goto-char (if back
                       (overlay-end visual--overlay)
                     (overlay-start visual--overlay)))
        (if back (backward-char) (forward-char)))
      (setq range (sp-get-thing back)
            beg (plist-get range :beg)
            end (plist-get range :end)))
    (when range
      (goto-char (if back beg end))
      (move-overlay visual--overlay beg end))
    range))

(defun visual-down-end ()
  (interactive)
  (visual-down t))

(defun visual-up-or-backwards ()
  (interactive)
  )

(defun visual-raise ()
  (interactive)
  (let* ((start (overlay-start visual--overlay))
         (end (overlay-end visual--overlay))
         (text (buffer-substring start end))
         (enclosing (sp-get-enclosing-sexp))
         (enc-start (plist-get enclosing :beg))
         (enc-end (plist-get enclosing :end)))
    (when enclosing
      (delete-region enc-start enc-end)
      ;; Record point before we insert
      (setq start (point))
      (insert text)
      (move-overlay visual--overlay start (point)))))

(defmacro visual-lisp-state-enter-command (command)
  "Wrap COMMAND to call evil-lisp-state before executing COMMAND."
  (let ((funcname (if (string-match "lisp-state-"
                                    (symbol-name command))
                      (intern (format "evil-%s" command))
                    (intern (format "evil-lisp-state-%s" command)))))
    `(progn
       (defun ,funcname ()
         (interactive)
         (unless visual--overlay
           (setq visual--overlay (make-overlay 1 1)))
         (call-interactively ',command)
         (when (and (not (evil-lisp-state-p))
                    evil-lisp-state-enter-lisp-state-on-command)
           (evil-lisp-state)))
       ',funcname)))

(let ((bindings
       '(("l" . visual-forward-sexp)
         ("h" . visual-backward-sexp)
         ("k" . visual-up)
         ("j" . visual-down)
         ("J" . visual-down-end)
         ("H" . visual-backward-shift-sexp)
         ("L" . visual-forward-shift-sexp)
         ("v" . visual-select-overlay)
         ("r" . visual-raise)
         ("o" . visual-exchange-end-and-beg))))
  (dolist (x bindings)
    (let ((key (car x))
          (cmd (cdr x)))
      (message "cmd: %s" cmd)
      (eval `(progn
              (define-key evil-lisp-state-map ,(kbd key)
                (visual-lisp-state-enter-command ,cmd)))))))

(define-key evil-lisp-state-map (kbd dotspacemacs-leader-key) spacemacs-default-map)
(define-key evil-lisp-state-map (kbd dotspacemacs-major-mode-leader-key)
  (lambda () (interactive) (symbol-value (intern (format "spacemacs-%s-map" major-mode)))))

(setq evil-lisp-state-cursor (list "HotPink1" 'bar))

;; (define-key evil-lisp-state-map (kbd "SPC") 'visual-select-overlay)

(defun select-symbol ()
    (interactive)
    (visual-select-range (sp-get-symbol)))

(define-key evil-motion-state-map "v" 'select-symbol)

;;
;; left right, move through the children 
;; move up and down the hiearchy
