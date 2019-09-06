;; (require 'package)
;; (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                     (not (gnutls-available-p))))
;;        (proto (if no-ssl "http" "https")))
;;   (when no-ssl
;;     (warn "\
;; Your version of Emacs does not support SSL connections,
;; which is unsafe because it allows man-in-the-middle attacks.
;; There are two things you can do about this warning:
;; 1. Install an Emacs version that does support SSL and be safe.
;; 2. Remove this warning from your init file so you won't see it again."))
;;   ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
;;   ;; (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
;;   (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
;;   (when (< emacs-major-version 24)
;;     ;; For important compatibility libraries like cl-lib
;;     (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; forward-to-word, backward-to-word for vi-like w, e, zap-up-to-char for dt
(require 'misc)

;;; Defaults
;; Highlight trailing whitespace
(setq show-trailing-whitespace t)
(setq tab-width 4)
;; Indent with spaces
(setq indent-tabs-mode nil)
;; Make 'kill-line also delete the \n character
(setq kill-whole-line t)
(setq c-basic-offset 4)
(setq tab-width 4)

;; Prompt for compile command when calling `compile`
(setq compilation-read-command t)
(setq compilation-scroll-output 'first-error)
(setq cjh-scroll-margin 5)
(setq scroll-margin cjh-scroll-margin)
(setq pop-up-windows nil)
;; Don't create "backup~" files
(setq make-backup-files nil)
;; Select window where help appears
(setq help-window-select t)

;; Save layout and buffers between sessions
(desktop-save-mode 1)
(electric-pair-mode 1)
;; Don't blink the cursor
(blink-cursor-mode 0)
;; No extra stuff in window
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

(defvar cjh-toggle-tab nil
  "Used by cjh-toggle-prev-buffer to toggle between next and previous buffer.")

(defvar cjh-last-searched-string nil
  "Stores the last searched string so that 'n' and 'N' can cycle through it.")

(defvar cjh-mode-exclusion-list '()
  "Major modes that shouldn't enable cjh-mode")
(setq cjh-mode-exclusion-list '(help-mode info-mode))

(defvar cjh-command-state t
  "t when in normal mode, nil when in insert mode")

;; TODO(chogan): cjh-mode makes its own map: cjh-mode-map.
;; Do I need to create a custom one?
(defvar cjh-keymap (make-sparse-keymap))
(defvar cjh-org-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map cjh-keymap)
    map))

(defun cjh-insert-state ()
  (interactive)
  (setq-local cursor-type 'bar)
  (cjh-insert-cursor-color)
  (setq-local overriding-local-map nil)
  (setq-local cjh-command-state nil))

(defun cjh-normal-state ()
  (interactive)
  (setq-local cursor-type 'box)
  (cjh-normal-cursor-color)
  (setq-local overriding-local-map cjh-keymap)
  (setq-local cjh-command-state t))

(define-minor-mode cjh-mode nil nil nil nil
  (unless (or
           (memq major-mode cjh-mode-exclusion-list)
           buffer-read-only)
    (if cjh-mode
        (cjh-normal-state)
      (cjh-insert-state))))

(setq cjh-escape-key-sequence "fd")
(setq cjh-escape-delay 0.1)
(setq cjh-previous-input nil)
(setq cjh-escape-timer-is-live nil)

;; 'fd' to go from insert to normal mode
;; TODO(chogan): minor mode for this?
(add-hook 'pre-command-hook 'cjh-escape-pre-command-hook)

(defun cjh-disable-escape-timer ()
  (setq cjh-escape-timer-is-live nil))

(defun cjh-escape-post-command-hook ()
  (backward-delete-char-untabify 2)
  (cjh-normal-state)
  (push-mark-command nil)
  (remove-hook 'post-command-hook 'cjh-escape-post-command-hook))

(defun cjh-escape-pre-command-hook ()
  (let ((this-key (aref (this-command-keys-vector) 0)))
    (when (and (eq cjh-command-state nil)
               (numberp this-key))
      (when (= this-key (aref cjh-escape-key-sequence 0))
        (setq cjh-escape-timer-is-live t)
        (run-at-time cjh-escape-delay nil 'cjh-disable-escape-timer))
      (when (and (= this-key (aref cjh-escape-key-sequence 1))
                 (= cjh-previous-input (aref cjh-escape-key-sequence 0))
                 cjh-escape-timer-is-live)
        (add-hook 'post-command-hook 'cjh-escape-post-command-hook))
      (setq cjh-previous-input this-key))))

;;; Keybindings
(define-key cjh-keymap "a" 'cjh-forward-and-insert)
(define-key cjh-keymap "b" 'backward-word)
(define-key cjh-keymap "c" 'cjh-change)
(define-key cjh-keymap "d" 'cjh-delete)
(define-key cjh-keymap "e" 'cjh-end-of-word)
(define-key cjh-keymap "f" 'cjh-find-forward)
(define-key cjh-keymap "gd" 'xref-find-definitions)
(define-key cjh-keymap "ge" 'backward-to-word)
(define-key cjh-keymap "gg" 'beginning-of-buffer)
(define-key cjh-keymap "h" 'backward-char)
(define-key cjh-keymap "i" 'cjh-insert-state)
(define-key cjh-keymap "j" 'next-line)
(define-key cjh-keymap "k" 'previous-line)
(define-key cjh-keymap "l" 'forward-char)
(define-key cjh-keymap "m" 'cjh-store-mark)
(define-key cjh-keymap "n" 'cjh-isearch-next)
(define-key cjh-keymap "o" 'cjh-open-newline-below)
(define-key cjh-keymap "p" 'cjh-paste)
;; q 'cjh-quit-search-mode
(define-key cjh-keymap "r" 'cjh-replace-char)
;; s 'cjh-search or 'cjh-substitute-replace
;; TODO(chogan): Implement
(define-key cjh-keymap "t" 'cjh-find-forward-till)
(define-key cjh-keymap "u" 'undo)
(define-key cjh-keymap "v" 'cjh-visual-state)
(define-key cjh-keymap "w" 'forward-to-word)
;; TODO(chogan): xp should swap chars
(define-key cjh-keymap "x" 'cjh-forward-delete-char)
(define-key cjh-keymap "y" 'cjh-yank)
;; z

(define-key cjh-keymap "A" 'cjh-eol-insert)
(define-key cjh-keymap "B" 'cjh-backward-whitespace)
(define-key cjh-keymap "C" 'cjh-change-to-eol)
(define-key cjh-keymap "D" 'kill-line)
;; E
(define-key cjh-keymap "F" 'cjh-find-backward)
(define-key cjh-keymap "G" 'end-of-buffer)
;; H
(define-key cjh-keymap "I" 'cjh-insert-beginning-of-line)
(define-key cjh-keymap "J" 'cjh-delete-indentation)
;; K
;; L
(define-key cjh-keymap "M" 'move-to-window-line-top-bottom)
(define-key cjh-keymap "N" 'cjh-isearch-prev)
(define-key cjh-keymap "O" 'cjh-open-newline-above)
;; P
;; Q
;; R
;; S
;; TODO(chogan): Implement this
(define-key cjh-keymap "T" 'cjh-find-backward-till)
;; U
;; TODO(chogan): Not quite right
(define-key cjh-keymap "V" 'cjh-start-visual-line-selection)
(define-key cjh-keymap "W" 'cjh-forward-whitespace)
(define-key cjh-keymap "X" 'cjh-backward-delete-char)
;; Y
;; Z
;; ~
;; `
;; !
;; @
;; #
(define-key cjh-keymap "$" 'cjh-move-to-end-of-line)
;; %
(define-key cjh-keymap "^" 'back-to-indentation)
;; &
(define-key cjh-keymap "*" 'isearch-forward-symbol-at-point)
;; *e
;; (
;; )
;; -
;; _
;; +
;; =
;; \
;; |
(define-key cjh-keymap ";" 'cjh-repeat-last-find)
(define-key cjh-keymap "'" 'cjh-goto-mark)
;; :
;; "
(define-key cjh-keymap "{" 'backward-paragraph)
(define-key cjh-keymap "}" 'forward-paragraph)
(define-key cjh-keymap "[ " 'cjh-newline-above)
(define-key cjh-keymap "] " 'cjh-newline-below)
;; <
;; >
;; ,
;; TODO(chogan): Improve semantics of this
(define-key cjh-keymap "." 'repeat)
;; TODO(chogan): Keep searched option after <RET> so I can n and N
;; through the results
(define-key cjh-keymap "/" 'cjh-isearch-forward)
(define-key cjh-keymap "?" 'cjh-isearch-backward)
(define-key cjh-keymap "0" 'beginning-of-line)

;; C-n 'cjh-multi-cursor-add
(define-key cjh-keymap (kbd "C-d") 'cjh-scroll-up-half)
(define-key cjh-keymap (kbd "C-o") 'pop-to-mark-command)
(define-key cjh-keymap (kbd "C-u") 'cjh-scroll-down-half)
;; C-v
(define-key cjh-keymap (kbd "C-;") 'cjh-insert-semicolon-at-eol)

;; Global keymaps
(global-set-key (kbd "C-;") 'cjh-insert-semicolon-at-eol)
(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-l") 'forward-char)

;;; SPC leader
(define-key cjh-keymap "  " 'execute-extended-command)
;; " a"
(define-key cjh-keymap " bb" 'switch-to-buffer)
(define-key cjh-keymap " bB" 'switch-to-buffer-other-window)
(define-key cjh-keymap " bd" 'kill-buffer)
(define-key cjh-keymap " bD" 'clean-buffer-list)
(define-key cjh-keymap " bp" 'previous-buffer)
(define-key cjh-keymap " bn" 'next-buffer)
;; " c"
(define-key cjh-keymap " d" 'dired)
(define-key cjh-keymap " en" 'compilation-next-error-function)
(define-key cjh-keymap " ff" 'ido-find-file)
(define-key cjh-keymap " fF" 'ido-find-file-other-window)
(define-key cjh-keymap " fr" 'ido-find-file-read-only)
;; (define-key cjh-keymap " fR" 'cjh-rename-file)
(define-key cjh-keymap " gg" 'goto-line)
(define-key cjh-keymap " ha" 'apropos-command)
(define-key cjh-keymap " hb" 'describe-bindings)
(define-key cjh-keymap " hc" 'describe-key-briefly)
(define-key cjh-keymap " hd" 'apropos-documentation)
(define-key cjh-keymap " hf" 'describe-function)
(define-key cjh-keymap " hi" 'info)
(define-key cjh-keymap " hk" 'describe-key)
(define-key cjh-keymap " hm" 'describe-mode)
(define-key cjh-keymap " ho" 'describe-symbol)
(define-key cjh-keymap " hr" 'info-emacs-manual)
(define-key cjh-keymap " hv" 'describe-variable)
(define-key cjh-keymap " hw" 'where-is)
(define-key cjh-keymap " h?" 'help-for-help)
(define-key cjh-keymap " hF" 'Info-goto-emacs-command-node)
(define-key cjh-keymap " hK" 'Info-goto-emacs-key-command-node)
(define-key cjh-keymap " hP" 'describe-package)
(define-key cjh-keymap " hS" 'info-lookup-symbol)
;; " i"
;; " j"
;; " k"
(setq list-directory-verbose-switches "-la")
;; (define-key cjh-keymap " ll" 'cjh-list-directory)
(define-key cjh-keymap " ls" 'list-directory)
;; " m"
;; " n"
;; " o"
;; " p"
(define-key cjh-keymap " q" 'save-buffers-kill-terminal)
(define-key cjh-keymap " r" 'cjh-reload-init-file)
;; " ry"
;; " s"
;; s/.../.../g
(define-key cjh-keymap " tn" 'linum-mode)
(define-key cjh-keymap " tw" 'whitespace-mode)
(define-key cjh-keymap " u" 'universal-argument)
;; " v"
(define-key cjh-keymap " wd" 'delete-window)
(define-key cjh-keymap " wh" 'other-window)
(define-key cjh-keymap " wl" 'other-window)
(define-key cjh-keymap " w/" 'split-window-right)
;; " x"
;; " y"
;; " z"
;; " /" project wide search
(define-key cjh-keymap " \t" 'cjh-toggle-prev-buffer)

;; TODO(chogan): Surround with ("[{'

;;; , leader
(define-key cjh-keymap ",b" 'compile)
(define-key cjh-keymap ",c" 'cjh-insert-if0-comment)
;; Align comments to fill-column
(define-key cjh-keymap ",f" 'fill-paragraph)
(define-key cjh-keymap ",gb" 'c-beginning-of-defun)
(define-key cjh-keymap ",ge" 'c-end-of-defun)
(define-key cjh-keymap ",mf" 'mark-defun)
(define-key cjh-keymap ",n" 'cjh-insert-note)
(define-key cjh-keymap ",r" 'recompile)
;; Surround region with if statement
(define-key cjh-keymap ",si" 'cjh-wrap-region-in-if)
(define-key cjh-keymap ",t" 'cjh-insert-todo)
(define-key cjh-keymap ",w" 'save-buffer)


;;; Theme
(add-to-list 'default-frame-alist '(font . "Liberation Mono-11.5"))
(set-face-attribute 'default t :font "Liberation Mono-11.5")

(defun true-color-p ()
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))

(defun cjh-insert-cursor-color ()
  (set-cursor-color "#7cfc00"))

(defun cjh-normal-cursor-color ()
  (set-cursor-color (if (true-color-p) "#ffcd48" "#d0d0d0")))

(defun cjh-set-help-cursor ()
  (set-cursor-color (if (true-color-p) "#87ceeb" "#00ccff")))


;; Display hex string in color it specifies
;; emacswiki.org/emacs/Hexcolour
(defvar hexcolour-keywords
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property (match-beginning 0)
                            (match-end 0)
			    'face (list :background
                            (match-string-no-properties 0)))))))
(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))
(add-hook 'prog-mode-hook 'hexcolour-add-to-font-lock)
;;

;;; Functions
;; (defun cjh-rename-file ()
;;   (interactive)
;;   (let ((this-file (buffer-file-name)))
;;     ;; TODO(chogan): Prompt for file name and append to path dir of this-file
;;     (rename-file this-file (read-string ...))))

(defun cjh-toggle-prev-buffer ()
  (interactive)
  (if cjh-toggle-tab
      (progn
        (setq cjh-toggle-tab nil)
        (next-buffer))
    (progn
      (setq cjh-toggle-tab t)
      (previous-buffer))))

(defun cjh-reload-init-file ()
  (interactive)
  (load-file user-init-file))

(defun cjh-eol-insert ()
  (interactive)
  (move-end-of-line nil)
  (cjh-insert-state))

(defun cjh-kill-word ()
  (interactive)
  (kill-word 1))

;; TODO(chogan):
;; (defun cjh-kill-to-end-of-word ()
;;   (interactive))

(defun cjh-kill-line ()
  (interactive)
  (move-beginning-of-line nil)
  (kill-line))

(defun cjh-kill-line-leave-newline ()
  (interactive)
  (let ((kill-whole-line nil))
    (move-beginning-of-line nil)
    (kill-line)))

(defun cjh-copy-line ()
  (interactive)
  (let ((old-pos (point)))
    (kill-whole-line)
    (yank)
    (goto-char old-pos)))

(defun cjh-forward-delete-char ()
  (interactive)
  (delete-forward-char 1 t))

(defun cjh-backward-delete-char ()
  (interactive)
  (backward-delete-char-untabify 1 t))

(defun cjh-forward-and-insert ()
  (interactive)
  (forward-char)
  (cjh-insert-state))

(defun cjh-move-to-end-of-line ()
  (interactive)
  (move-end-of-line nil))

(defun cjh-window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun cjh-scroll-up-half ()
  (interactive)
  (scroll-up (cjh-window-half-height))
  (move-to-window-line (cjh-window-half-height)))

(defun cjh-scroll-down-half ()
  (interactive)
  (scroll-down (cjh-window-half-height))
  (move-to-window-line (cjh-window-half-height)))

(defun cjh-newline-above ()
  (interactive)
  (save-excursion
    (previous-line)
    (move-end-of-line nil)
    (newline)))

(defun cjh-newline-below ()
  (interactive)
  (save-excursion
    (move-end-of-line nil)
    (newline)))

(defun cjh-open-newline-above ()
  (interactive)
  (previous-line)
  (move-end-of-line nil)
  (newline)
  (cjh-insert-state))

(defun cjh-open-newline-below ()
  (interactive)
  (move-end-of-line nil)
  (newline)
  (cjh-insert-state))

(defun cjh-change-word ()
  (interactive)
  (kill-word 1)
  (cjh-insert-state))

(defun cjh-change-inner-word ()
  (interactive)
  (backward-word)
  (cjh-change-word))

(defun cjh-change-to-eol ()
  (interactive)
  (let ((kill-whole-line nil))
    (kill-line)
    (cjh-insert-state)))

(defun cjh-insert-todo ()
  (interactive)
  (indent-for-tab-command)
  (comment-dwim nil)
  (insert "TODO(chogan): ")
  (cjh-insert-state))

(defun cjh-insert-note ()
  (interactive)
  (indent-for-tab-command)
  (comment-dwim nil)
  (insert "NOTE(chogan): ")
  (cjh-insert-state))

(defun cjh-insert-semicolon-at-eol ()
  (interactive)
  (cjh-eol-insert)
  (insert ";")
  (cjh-normal-state))

(defun cjh-insert-beginning-of-line ()
  (interactive)
  (back-to-indentation)
  (cjh-insert-state))

(defun cjh-end-of-word ()
  (interactive)
  (forward-char)
  (forward-word)
  (backward-char))

(defvar cjh-last-find-char nil)
(defvar cjh-last-end-of-find-point nil)
(defvar cjh-last-find-direction nil)

(defun cjh-find-forward ()
  (interactive)
  (let ((char (char-to-string (read-char)))
        (end-of-find-point (save-excursion (end-of-line) (point))))
    (if (search-forward char end-of-find-point t)
        (progn
          (backward-char)
          (setq cjh-last-find-char char)
          (setq cjh-last-end-of-find-point end-of-find-point)
          (setq cjh-last-find-direction 1)))))

(defun cjh-repeat-last-find ()
  (interactive)
  (if (eq cjh-last-find-direction 1)
      (cjh-repeat-last-find-forward)
    (cjh-repeat-last-find-backward)))

(defun cjh-repeat-last-find-forward ()
  (interactive)
  (forward-char)
  (if (search-forward cjh-last-find-char cjh-last-end-of-find-point t)
      (backward-char)))

(defun cjh-repeat-last-find-backward ()
  (interactive)
  (search-backward cjh-last-find-char cjh-last-end-of-find-point t))

(defun cjh-find-backward ()
  (interactive)
  (let ((char (char-to-string (read-char)))
        (end-of-find-point (save-excursion (beginning-of-line) (point))))
    (if (search-backward char end-of-find-point t)
        (progn
          (setq cjh-last-find-char char)
          (setq cjh-last-end-of-find-point end-of-find-point)
          (setq cjh-last-find-direction 0)))))

(defun cjh-find-forward-till ()
  (interactive))

(defun cjh-find-backward-till ()
  (interactive))

(defun cjh-isearch-forward ()
  (interactive)
  (isearch-forward))

(defun cjh-isearch-backward ()
  (interactive)
  (isearch-backward))

;; TODO(chogan): Broken
(defun cjh-isearch-next ()
  (interactive)
  (if cjh-isearch-state
      (isearch-repeat-forward)))

;; TODO(chogan): Broken
(defun cjh-isearch-prev ()
  (interactive)
  (if cjh-isearch-state
      (isearch-repeat-backward)))

(defun cjh-delete-indentation ()
  (interactive)
  (delete-indentation t))

(defvar cjh-insert-if0 t)

(defun cjh-insert-if0-comment ()
  (interactive)
  (if cjh-insert-if0
      (progn
        (insert "#if 0")
        (setq cjh-insert-if0 nil))
    (progn
      (insert "#endif")
      (setq cjh-insert-if0 t))))

(defun cjh-replace-char (char)
  (interactive "c")
  (delete-char 1)
  (insert-char char 1)
  (backward-char 1))

(defun cjh-forward-whitespace ()
  (interactive)
  (forward-whitespace 1))

(defun cjh-backward-whitespace ()
  (interactive)
  (forward-whitespace -1))

(defun cjh-store-mark (char)
  (interactive "c")
  (point-to-register char))

(defun cjh-goto-mark (char)
  (interactive "c")
  (jump-to-register char))

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

(defun cjh-c-mode-font-lock-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

(defun cjh-c-mode-common-hook ()
  (font-lock-add-keywords
   nil
   '((cjh-c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

(defun cjh-paste ()
  "Paste with special newline handling.

If the text to paste ends with a newline, open a newline below
the line at point and insert the line there."
  (interactive)
  (let ((to-paste (car kill-ring-yank-pointer)))
    (if (string-match "\n$" to-paste)
        (progn
          (next-line)
          (beginning-of-line)
          (yank))
      (yank))))

(defun cjh-delete (beg end)
  (interactive "r")
  (if (use-region-p)
      (kill-region beg end)
    (let ((char (read-char)))
      (cond
       ((char-equal ?d char) (cjh-kill-line))
       ((char-equal ?n char) (cjh-kill-line-leave-newline))
       ((char-equal ?w char) (cjh-kill-word))
       ;; TODO(chogan):
       ;; ((char-equal ?e char) (cjh-kill-to-end-of-word))
       ;; ((char-equal ?W char) (cjh-kill-whitespace-forward))
       ;; ((char-equal ?i char)
       ;;  (cond
       ;;   ((char-equal ?a char) ()
       ;;    )))
       ((char-equal ?l char) (delete-blank-lines))
       ((char-equal ?f char)
        (let ((second-char (read-char)))
          (zap-to-char 1 second-char)))
       ((char-equal ?t char)
        (let ((second-char (read-char)))
          (zap-up-to-char 1 second-char)))))))

(defun cjh-change ()
  (interactive)
  (let ((char (read-char)))
    (cond
     ((char-equal ?w char) (cjh-change-word))
     ;; TODO(chogan):
     ;; ((char-euqal ?W char) ())
     ;; ((char-euqal ?e char) ())
     ;; ((char-euqal ?E char) ())
     ((char-equal ?f char)
      (let ((second-char (read-char)))
        (zap-to-char 1 second-char)
        (cjh-insert-state)))
     ((char-equal ?t char)
      (let ((second-char (read-char)))
        (zap-up-to-char 1 second-char)
        (cjh-insert-state)))
     ((char-equal ?i char)
      (let ((second-char (read-char)))
        (cond
         ((char-equal ?w second-char) (cjh-change-inner-word))
         ;; TODO(chogan):
         ;; ((char-equal ?a second-char) ())
         )))
     )))

(defun cjh-yank ()
  (interactive)
  (if (use-region-p)
      (call-interactively 'kill-ring-save)
    (let ((char (read-char)))
      (cond
       ((char-equal ?y char) (cjh-copy-line))
       ;; TODO(chogan):
       ;; ((char-equal ?w char) ())
       ;; ((char-equal ?e char) ())
       ;; ((char-equal ?b char) ())
       ((char-equal ?f char)
        (let ((second-char (read-char)))
          (zap-to-char 1 second-char)
          (yank)))
       ((char-equal ?t char)
        (let ((second-char (read-char)))
          (zap-up-to-char 1 second-char)
          (yank)))
       ((char-equal ?i char)
        (let ((second-char (read-char)))
          (cond
           ;; TODO(chogan):
           ;; ((char-equal ?w char) (cjh-copy-word))
           )))
       ))))

(defun cjh-quit-help ()
  (interactive)
  (cjh-normal-state)
  (quit-window))

(defun cjh-start-visual-line-selection ()
  (interactive)
  (beginning-of-line)
  (call-interactively 'set-mark-command)
  (end-of-line))

(defun cjh-visual-state ()
  (interactive)
  (call-interactively 'set-mark-command))

(defun cjh-use-org-keymap ()
  (interactive)
  (setq-local overriding-local-map 'cjh-org-keymap))

(with-eval-after-load "org"
  (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars t)
  ;; NOTE(chogan): These don't take effect when set in cjh-theme.el
  (set-face-attribute 'outline-1 nil :height 1.4 :foreground "#4f97d7")
  (set-face-attribute 'outline-2 nil :height 1.3 :foreground "#2d9574")
  (set-face-attribute 'outline-3 nil :height 1.2 :foreground "#67b11d")
  (set-face-attribute 'outline-4 nil :height 1.1 :foreground "#b1951d")
  (set-face-attribute 'outline-5 nil :height 1.0 :foreground "#4f97d7")
  (set-face-attribute 'outline-6 nil :height 1.0 :foreground "#2d9574")
  (set-face-attribute 'outline-7 nil :height 1.0 :foreground "#67b11d")
  (set-face-attribute 'outline-8 nil :height 1.0 :foreground "#b1951d")
  (set-face-attribute 'org-block nil :background "#2f2b33" :foreground "#cbc1d5")
  (set-face-attribute 'org-meta-line nil :foreground "#9f8766")
  (set-face-attribute 'org-block-begin-line nil :background "#373040" :foreground "#827591")
  (set-face-attribute 'org-block-end-line nil :background "#373040" :foreground "#827591")
  (set-face-attribute 'org-code nil :foreground "#28def0")
  (define-key cjh-org-keymap " ma" 'org-agenda)
  (define-key cjh-org-keymap " ms" 'org-schedule)
  (define-key cjh-org-keymap " mr" 'org-clock-report)
  (define-key cjh-org-keymap " mI" 'org-clock-in)
  (define-key cjh-org-keymap " mO" 'org-clock-out)
  (define-key cjh-org-keymap " mb" 'org-insert-structure-template)
  (define-key cjh-org-keymap "t" 'org-todo))

(defun cjh-init-org ()
  (enable-cjh-mode)
  (cjh-use-org-keymap))

;; TODO(chogan):
(defun cjh-copy-word ()
  (interactive))

(defun enable-cjh-mode ()
  (cjh-mode 1))

;;; Hooks
;; Should never be in normal state in the minibuffer
(add-hook 'minibuffer-setup-hook 'cjh-insert-state)
(add-hook 'minibuffer-exit-hook 'cjh-normal-state)
;; Comment out #if 0 blocks
(add-hook 'c-mode-common-hook 'cjh-c-mode-common-hook)
;; Don't indent when inside a namespace
(add-hook 'c++-mode-hook (lambda () (c-set-offset 'innamespace [0])))
;; Include underscores in word
(add-hook 'python-mode-hook (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'c-mode-common-hook (lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'help-mode-hook 'cjh-set-help-cursor)
(add-hook 'help-mode-hook (lambda () (local-set-key "q" 'cjh-quit-help)))
;; (add-hook 'isearch-mode-end-hook (lambda () (setq cjh-isearch-state nil)))
(add-hook 'prog-mode-hook 'enable-cjh-mode)
(add-hook 'org-mode-hook 'cjh-init-org)
(add-hook 'messages-buffer-mode-hook 'enable-cjh-mode)

(add-to-list 'custom-theme-load-path "~/.emacs.d/cjh")
(load-theme 'cjh-theme t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style (quote ((c-mode . "linux") (c++-mode . "linux")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
