;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;; Defaults
;; Highlight trailing whitespace
(setq-default show-trailing-whitespace t)
(setq-default tab-width 4)
;; Indent with spaces
(setq-default indent-tabs-mode nil)
;; Make 'kill-line also delete the \n character
(setq-default kill-whole-line t)
(setq c-basic-offset 4)

;; Save layout and buffers between sessions
(desktop-save-mode 1)
(blink-cursor-mode 0)

;; Major modes that shouldn't enable cjh-mode
(defvar cjh-mode-exclusion-list '())

(setq cjh-mode-exclusion-list '('help-mode))

;;; Modes
(defvar cjh-command-state t)
(defvar cjh-keymap (make-sparse-keymap))

(defun cjh-insert-state ()
  (interactive)
  (setq-local cursor-type 'bar)
  (setq overriding-local-map nil)
  (setq cjh-command-state nil))

(defun cjh-normal-state ()
  (interactive)
  (setq-local cursor-type 'box)
  (setq overriding-local-map cjh-keymap)
  (setq cjh-command-state t))

(define-minor-mode cjh-mode nil nil nil nil
  (unless (or (minibufferp)
              (member major-mode cjh-mode-exclusion-list))
    (cjh-normal-state)))

(define-key cjh-keymap "i" 'cjh-insert-state)
;; TODO(chogan): Change this to fd
(global-set-key (kbd "M-e") 'cjh-normal-state)

;; Motions
(define-key cjh-keymap "h" 'backward-char)
(define-key cjh-keymap "j" 'next-line)
(define-key cjh-keymap "k" 'previous-line)
(define-key cjh-keymap "l" 'forward-char)
(define-key cjh-keymap ",c" 'cjh-wrap-region-in-if)
(define-key cjh-keymap ",w" 'save-buffer)
(define-key cjh-keymap "A" 'cjh-eol-insert)
(define-key cjh-keymap "w" 'forward-word)
;; e
(define-key cjh-keymap "b" 'backward-word)
(define-key cjh-keymap "$" 'cjh-move-to-end-of-line)
(define-key cjh-keymap "^" 'back-to-indentation)
;; 0

;; Editing
(define-key cjh-keymap "dd" 'cjh-kill-line)
;; TODO(chogan): "dd" that doesn't kill the newline
(define-key cjh-keymap (kbd "S-d") 'kill-line)
;; C
(define-key cjh-keymap "yy" 'cjh-copy-line)
;; TODO(chogan): Doesn't insert the newline
(define-key cjh-keymap "p" 'yank)
(define-key cjh-keymap " q" 'save-buffers-kill-terminal)
(define-key cjh-keymap " r" 'cjh-reload-init-file)
(define-key cjh-keymap " b" 'switch-to-buffer)
(define-key cjh-keymap "x" 'cjh-forward-delete-char)
(define-key cjh-keymap "u" 'undo)
(define-key cjh-keymap "a" 'cjh-forward-and-insert)

;; r
;; o

;; d...
;; y...
;; c...

;; Buffer commands
;; " bb"
;; " bd"

;; Window commands
(define-key cjh-keymap " wl" 'other-window)
(define-key cjh-keymap " wh" 'other-window)
(define-key cjh-keymap " w/" 'split-window-right)
(define-key cjh-keymap " wq" 'delete-window)

;;; Visuals
(add-to-list 'default-frame-alist '(font . "Liberation Mono-11.5"))
(set-face-attribute 'default t :font "Liberation Mono-11.5")

(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-type-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")

;;; Functions
(defun cjh-reload-init-file ()
  (interactive)
  (load-file user-init-file))

(defun cjh-eol-insert ()
  (interactive)
  (move-end-of-line nil)
  (cjh-insert-state))

(defun cjh-kill-line ()
  (interactive)
  (move-beginning-of-line nil)
  (kill-line))

(defun cjh-copy-line ()
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (copy-region-as-kill beg end)))
(defun cjh-forward-delete-char ()
  (interactive)
  (backward-forward-char-untabify 1 t))

(defun cjh-forward-and-insert ()
  (interactive)
  (forward-char)
  (cjh-insert-state))
(defun cjh-move-to-end-of-line ()
  (interactive)
  (move-end-of-line nil))

(defun cjh-wrap-region-in-if (start end)
  (interactive "r")
  (let ((num-lines (count-lines start end)))
    (goto-char start)
    (newline)
    (newline)
    (previous-line)
    (previous-line)
    (indent-for-tab-command)
    (insert "if ()")
    ;; TODO(chogan): Not sure why the previous insertion adds extra whitespace.
    (delete-trailing-whitespace (line-beginning-position) (line-end-position))
    (next-line)
    (insert "{")
    (indent-for-tab-command)
    (dotimes (line num-lines '())
      (next-line)
      (indent-for-tab-command))
    (end-of-line)
    (newline)
    (insert "}")
    (indent-for-tab-command)
    (dotimes (line (+ 2 num-lines) '())
      (previous-line))
    (search-forward "(")))

(defun post-load-stuff ()
  (interactive)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-foreground-color "burlywood3")
  (set-background-color "#161616")
  (set-cursor-color "#40FF40"))

;;; hooks
(add-hook 'window-setup-hook 'post-load-stuff t)

;;; Keybindings


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style
   (quote
    ((c-mode . "linux")
     (c++-mode . "linux")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "linux"))))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(make-backup-files nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
