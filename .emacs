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

;; Major modes that shouldn't enable cjh-mode
(defvar cjh-mode-exclusion-list '())
(setq cjh-mode-exclusion-list (list 'help-mode 'info-mode))

(defvar cjh-isearch-state
  "t when within an isearch, otherwise nil."
  nil)

;;; Modes
(defvar cjh-command-state t)
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

(define-key cjh-keymap "i" 'cjh-insert-state)

;;; Motions
(define-key cjh-keymap "h" 'backward-char)
(define-key cjh-keymap "j" 'next-line)
(define-key cjh-keymap "k" 'previous-line)
(define-key cjh-keymap "l" 'forward-char)
(define-key cjh-keymap "A" 'cjh-eol-insert)
(define-key cjh-keymap "w" 'forward-word)
(define-key cjh-keymap "b" 'backward-word)
(define-key cjh-keymap "W" 'cjh-forward-whitespace)
(define-key cjh-keymap "B" 'cjh-backward-whitespace)
(define-key cjh-keymap "$" 'cjh-move-to-end-of-line)
(define-key cjh-keymap "^" 'back-to-indentation)
(define-key cjh-keymap (kbd "C-d") 'cjh-scroll-up-half)
(define-key cjh-keymap (kbd "C-u") 'cjh-scroll-down-half)
(define-key cjh-keymap "gg" 'beginning-of-buffer)
(define-key cjh-keymap "G" 'end-of-buffer)
(define-key cjh-keymap "M" 'move-to-window-line-top-bottom)
(define-key cjh-keymap "e" 'cjh-end-of-word)
(define-key cjh-keymap "0" 'beginning-of-line)
(define-key cjh-keymap "f" 'cjh-find-forward)
(define-key cjh-keymap "F" 'cjh-find-backward)
(define-key cjh-keymap "t" 'cjh-find-forward-till)
(define-key cjh-keymap "T" 'cjh-find-backward-till)
(define-key cjh-keymap ";" 'cjh-repeat-last-find)
(define-key cjh-keymap " gg" 'goto-line)
(define-key cjh-keymap "gd" 'xref-find-definitions)
(define-key cjh-keymap "m" 'cjh-store-mark)
(define-key cjh-keymap "'" 'cjh-goto-mark)
(define-key cjh-keymap (kbd "C-o") 'pop-to-mark-command)

;;; Editing
(define-key cjh-keymap "D" 'kill-line)
(define-key cjh-keymap "p" 'cjh-paste)
(define-key cjh-keymap " q" 'save-buffers-kill-terminal)
(define-key cjh-keymap " r" 'cjh-reload-init-file)
(define-key cjh-keymap "x" 'cjh-forward-delete-char)
(define-key cjh-keymap "X" 'cjh-backward-delete-char)
(define-key cjh-keymap "u" 'undo)
(define-key cjh-keymap "a" 'cjh-forward-and-insert)
(define-key cjh-keymap "[ " 'cjh-newline-above)
(define-key cjh-keymap "] " 'cjh-newline-below)
(define-key cjh-keymap "O" 'cjh-open-newline-above)
(define-key cjh-keymap "o" 'cjh-open-newline-below)
(define-key cjh-keymap "I" 'cjh-insert-beginning-of-line)
(define-key cjh-keymap "r" 'cjh-replace-char)
(define-key cjh-keymap "J" 'cjh-delete-indentation)
;; TODO(chogan): Improve semantics of this
(define-key cjh-keymap "." 'repeat)
;; s/.../.../g
;; " ry"

;; Surround with ("[{
;; Text Objects
(define-key cjh-keymap "d" 'cjh-delete)
(defun cjh-delete (beg end)
  (interactive "r")
  (if (use-region-p)
      (kill-region beg end)
    (let ((char (read-char)))
      (cond
       ((char-equal ?d char) (cjh-kill-line))
       ((char-equal ?n char) (cjh-kill-line-leave-newline))
       ((char-equal ?w char) (cjh-kill-word))
       ;; ((char-equal ?W char) (cjh-kill-whitespace-forward))
       ((char-equal ?l char) (delete-blank-lines))))))
;; dW
;; dia
;; df...
;; dt...
(define-key cjh-keymap "y" 'cjh-yank)
(defun cjh-yank (beg end)
  (interactive "r")
  (if (use-region-p)
      (message "Saved %s to %s" beg end)
      (kill-ring-save beg end)
    (let ((char (read-char)))
      (cond
       ((char-equal ?y char) (cjh-copy-line))
       ))))

;; yw
;; ye
;; yiw
;; yb
(define-key cjh-keymap "cw" 'cjh-change-word)
(define-key cjh-keymap "ciw" 'cjh-change-inner-word)
(define-key cjh-keymap "C" 'cjh-change-to-eol)
;; cW
;; ce
;; cE
;; cia
;; cf...
;; ct...

;; (fill-paragraph) M-q - After writing a long one line comment, format to fill line
;; (fill-region) - Realign comment block to fill-column

(define-key cjh-keymap " u" 'universal-argument)

;; Visual Mode
(define-key cjh-keymap "v" 'cjh-visual-state)
;; TODO(chogan): Not quite right
(define-key cjh-keymap "V" 'cjh-start-visual-line-selection)
;; C-v

;; TODO(chogan):
;; (defvar cjh-visual-state nil)

(defun cjh-start-visual-line-selection ()
  (interactive)
  (beginning-of-line)
  (call-interactively 'set-mark-command)
  (end-of-line))

(defun cjh-visual-state ()
  (interactive)
  (call-interactively 'set-mark-command))

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

;; Multi-cursor mode
;; C-n
;; Try Doom emacs multi-cursor package?

;; Search
;; TODO(chogan): Keep searched option after <RET>
(defvar cjh-last-searched-string nil
  "Stores the last searched string so that 'n' and 'N' can cycle through it.")

(define-key cjh-keymap "/" 'cjh-isearch-forward)
(define-key cjh-keymap "?" 'cjh-isearch-backward)
(define-key cjh-keymap "n" 'cjh-isearch-next)
(define-key cjh-keymap "N" 'cjh-isearch-prev)
;; *
;; SPC /

;; iedit mode
;; * e

;; Compilation
(define-key cjh-keymap " en" 'compilation-next-error-function)

(define-key cjh-keymap "  " 'execute-extended-command)

;;; File Commands
(define-key cjh-keymap " ff" 'find-file)
(define-key cjh-keymap " fF" 'find-file-other-window)
(define-key cjh-keymap " fr" 'find-file-read-only)
;; (define-key cjh-keymap " fR" 'cjh-rename-file)

;;; Directories
(setq list-directory-verbose-switches "-la")
(define-key cjh-keymap " ls" 'list-directory)
;; (define-key cjh-keymap " ll" 'cjh-list-directory)
(define-key cjh-keymap " d" 'dired)

;;; Buffer commands
(define-key cjh-keymap " bb" 'switch-to-buffer)
(define-key cjh-keymap " bB" 'switch-to-buffer-other-window)
(define-key cjh-keymap " bd" 'kill-buffer)
(define-key cjh-keymap " bD" 'clean-buffer-list)
(define-key cjh-keymap " bp" 'previous-buffer)
(define-key cjh-keymap " bn" 'next-buffer)
(defvar cjh-toggle-tab nil
  "Used by cjh-toggle-prev-buffer to toggle between next and previous buffer.")
(define-key cjh-keymap " \t" 'cjh-toggle-prev-buffer)


;;; Window commands
(define-key cjh-keymap " wl" 'other-window)
(define-key cjh-keymap " wh" 'other-window)
(define-key cjh-keymap " w/" 'split-window-right)
(define-key cjh-keymap " wd" 'delete-window)

;;; Custom Bindings
(define-key cjh-keymap ",b" 'compile)
(define-key cjh-keymap ",c" 'cjh-insert-if0-comment)
(define-key cjh-keymap ",gb" 'c-beginning-of-defun)
(define-key cjh-keymap ",ge" 'c-end-of-defun)
(define-key cjh-keymap ",mf" 'mark-defun)
(define-key cjh-keymap ",n" 'cjh-insert-note)
(define-key cjh-keymap ",r" 'recompile)
;; Surround region with if statement
(define-key cjh-keymap ",si" 'cjh-wrap-region-in-if)
(define-key cjh-keymap ",t" 'cjh-insert-todo)
(define-key cjh-keymap ",w" 'save-buffer)
(define-key cjh-keymap (kbd "C-;") 'cjh-insert-semicolon-at-eol)

(global-set-key (kbd "C-;") 'cjh-insert-semicolon-at-eol)

;;; Help
(define-key cjh-keymap " hf" 'describe-function)
(define-key cjh-keymap " hv" 'describe-variable)
(define-key cjh-keymap " hm" 'describe-mode)
(define-key cjh-keymap " hc" 'describe-key-briefly)
(define-key cjh-keymap " ha" 'apropos-command)
(define-key cjh-keymap " hb" 'describe-bindings)
(define-key cjh-keymap " hd" 'apropos-documentation)
(define-key cjh-keymap " hF" 'Info-goto-emacs-command-node)
(define-key cjh-keymap " hi" 'info)
(define-key cjh-keymap " hk" 'describe-key)
(define-key cjh-keymap " hK" 'Info-goto-emacs-key-command-node)
(define-key cjh-keymap " ho" 'describe-symbol)
(define-key cjh-keymap " hr" 'info-emacs-manual)
(define-key cjh-keymap " hS" 'info-lookup-symbol)
(define-key cjh-keymap " hw" 'where-is)
(define-key cjh-keymap " h?" 'help-for-help)

;;; Visuals
(define-key cjh-keymap " tn" 'linum-mode)
(define-key cjh-keymap " tw" 'whitespace-mode)

;;; org-mode key bindings
;; schedule
;; report
;; clock in
;; clock out
;; t

;; From Casey
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

(defun cjh-create-theme (theme-name)
  (let ((class '((class color) (min-colors 89)))
        ;;                                   GUI     Terminal
        (act1          (if (true-color-p) "#222226" "#121212"))
        (act2          (if (true-color-p) "#5d4d7a" "#444444"))
        (base          (if (true-color-p) "#b2b2b2" "#b2b2b2"))
        (base-dim      (if (true-color-p) "#686868" "#585858"))
        (bg1           (if (true-color-p) "#292b2e" "#262626"))
        (bg2           (if (true-color-p) "#212026" "#1c1c1c"))
        (bg3           (if (true-color-p) "#100a14" "#121212"))
        (bg4           (if (true-color-p) "#0a0814" "#080808"))
        (border        (if (true-color-p) "#5d4d7a" "#111111"))
        (cblk          (if (true-color-p) "#cbc1d5" "#b2b2b2"))
        (cblk-bg       (if (true-color-p) "#2f2b33" "#262626"))
        (cblk-ln       (if (true-color-p) "#827591" "#af5faf"))
        (cblk-ln-bg    (if (true-color-p) "#373040" "#333333"))
        (cursor        (if (true-color-p) "#ffcd48" "#d0d0d0"))
        (const         (if (true-color-p) "#a45bad" "#d75fd7"))
        (comment       (if (true-color-p) "#2aa1ae" "#008787"))
        (comment-light (if (true-color-p) "#2aa1ae" "#008787"))
        (comment-bg    (if (true-color-p) "#292e34" "#262626"))
        (comp          (if (true-color-p) "#c56ec3" "#d75fd7"))
        (err           (if (true-color-p) "#e0211d" "#e0211d"))
        (func          (if (true-color-p) "#bc6ec5" "#d75fd7"))
        (head1         (if (true-color-p) "#4f97d7" "#268bd2"))
        (head1-bg      (if (true-color-p) "#293239" "#262626"))
        (head2         (if (true-color-p) "#2d9574" "#2aa198"))
        (head2-bg      (if (true-color-p) "#293235" "#262626"))
        (head3         (if (true-color-p) "#67b11d" "#67b11d"))
        (head3-bg      (if (true-color-p) "#293235" "#262626"))
        (head4         (if (true-color-p) "#b1951d" "#875f00"))
        (head4-bg      (if (true-color-p) "#32322c" "#262626"))
        (highlight     (if (true-color-p) "#444155" "#444444"))
        (highlight-dim (if (true-color-p) "#3b314d" "#444444"))
        (keyword       (if (true-color-p) "#4f97d7" "#268bd2"))
        (lnum          (if (true-color-p) "#44505c" "#444444"))
        (mat           (if (true-color-p) "#86dc2f" "#86dc2f"))
        (meta          (if (true-color-p) "#9f8766" "#af875f"))
        (str           (if (true-color-p) "#2d9574" "#2aa198"))
        (suc           (if (true-color-p) "#86dc2f" "#86dc2f"))
        (ttip          (if (true-color-p) "#9a9aba" "#888888"))
        (ttip-sl       (if (true-color-p) "#5e5079" "#333333"))
        (ttip-bg       (if (true-color-p) "#34323e" "#444444"))
        (type          (if (true-color-p) "#ce537a" "#df005f"))
        (var           (if (true-color-p) "#7590db" "#8787d7"))
        (war           (if (true-color-p) "#dc752f" "#dc752f"))

        ;; colors
        (aqua          (if (true-color-p) "#2d9574" "#2aa198"))
        (aqua-bg       (if (true-color-p) "#293235" "#262626"))
        (green         (if (true-color-p) "#67b11d" "#67b11d"))
        (green-bg      (if (true-color-p) "#293235" "#262626"))
        (green-bg-s    (if (true-color-p) "#29422d" "#262626"))
        (cyan          (if (true-color-p) "#28def0" "#00ffff"))
        (red           (if (true-color-p) "#f2241f" "#d70000"))
        (red-bg        (if (true-color-p) "#3c2a2c" "#262626"))
        (red-bg-s      (if (true-color-p) "#512e31" "#262626"))
        (blue          (if (true-color-p) "#4f97d7" "#268bd2"))
        (blue-bg       (if (true-color-p) "#293239" "#262626"))
        (blue-bg-s     (if (true-color-p) "#2d4252" "#262626"))
        (magenta       (if (true-color-p) "#a31db1" "#af00df"))
        (yellow        (if (true-color-p) "#b1951d" "#875f00"))
        (yellow-bg     (if (true-color-p) "#32322c" "#262626")))

    (custom-theme-set-faces
      theme-name

    ;; basics
    `(cursor ((,class (:background ,cursor))))
    `(custom-button ((,class :background ,bg2 :foreground ,base :box (:line-width 2 :style released-button))))
    `(default ((,class (:background ,bg1 :foreground ,base))))
    `(default-italic ((,class (:italic t))))
    `(error ((,class (:foreground ,err))))
    `(eval-sexp-fu-flash ((,class (:background ,suc :foreground ,bg1))))
    `(eval-sexp-fu-flash-error ((,class (:background ,err :foreground ,bg1))))
    `(font-lock-builtin-face ((,class (:foreground ,keyword))))
    `(font-lock-comment-face ((,class (:foreground ,comment :background nil :slant ,'normal))))
    `(font-lock-constant-face ((,class (:foreground ,const))))
    `(font-lock-doc-face ((,class (:foreground ,meta))))
    `(font-lock-function-name-face ((,class (:foreground ,func :inherit bold))))
    `(font-lock-keyword-face ((,class (:inherit bold :foreground ,keyword :slant ,'normal))))
    `(font-lock-negation-char-face ((,class (:foreground ,const))))
    `(font-lock-preprocessor-face ((,class (:foreground ,func))))
    `(font-lock-reference-face ((,class (:foreground ,const))))
    `(font-lock-string-face ((,class (:foreground ,str))))
    `(font-lock-type-face ((,class (:foreground ,type :inherit bold))))
    `(font-lock-variable-name-face ((,class (:foreground ,var))))
    `(font-lock-warning-face ((,class (:foreground ,war :background ,bg1))))
    `(fringe ((,class (:background ,bg1 :foreground ,base))))
    `(header-line ((,class :background ,bg2)))
    `(highlight ((,class (:foreground ,base :background ,highlight))))
    `(hl-line ((,class (:background ,bg2))))
    `(isearch ((,class (:foreground ,bg1 :background ,mat))))
    `(lazy-highlight ((,class (:background ,green-bg-s :weight normal))))
    `(link ((,class (:foreground ,comment :underline t))))
    `(link-visited ((,class (:foreground ,comp :underline t))))
    `(match ((,class (:background ,highlight :foreground ,mat))))
    `(minibuffer-prompt ((,class (:inherit bold :foreground ,keyword))))
    `(page-break-lines ((,class (:foreground ,act2))))
    `(region ((,class (:background ,highlight))))
    `(secondary-selection ((,class (:background ,bg3))))
    `(shadow ((,class (:foreground ,base-dim))))
    `(success ((,class (:foreground ,suc))))
    `(tooltip ((,class (:background ,ttip-sl :foreground ,base :bold nil :italic nil :underline nil))))
    `(vertical-border ((,class (:foreground ,border))))
    `(warning ((,class (:foreground ,war))))

    `(highlight-symbol-face ((,class (:background ,bg2))))

    ;;;;; show-paren
    `(show-paren-match ((,class (:foreground ,mat :inherit bold))))
    `(show-paren-match-expression ((,class (:background ,green-bg-s))))
    `(show-paren-mismatch ((,class (:foreground ,err :inherit bold)))))

    ;; org-mode
    `(org-level-1 ((,class (:inherit bold :bold ,nil :foreground ,head1 :height 2.0))))
    `(org-level-2 ((,class (:inherit bold :bold ,nil :foreground ,head2 :height 1.2))))
    `(org-level-3 ((,class (:bold nil :foreground ,head3 :height 1.1))))
    `(org-level-4 ((,class (:bold nil :foreground ,head4 ))))
    `(org-level-5 ((,class (:bold nil :foreground ,head1))))
    `(org-level-6 ((,class (:bold nil :foreground ,head2))))
    `(org-level-7 ((,class (:bold nil :foreground ,head3))))
    `(org-level-8 ((,class (:bold nil :foreground ,head4))))

    ;; outline
    `(outline-1 ((,class (:inherit org-level-1))))
    `(outline-2 ((,class (:inherit org-level-2))))
    `(outline-3 ((,class (:inherit org-level-3))))
    `(outline-4 ((,class (:inherit org-level-4))))
    `(outline-5 ((,class (:inherit org-level-5))))
    `(outline-6 ((,class (:inherit org-level-6))))
    `(outline-7 ((,class (:inherit org-level-7))))
    `(outline-8 ((,class (:inherit org-level-8))))

    (custom-theme-set-variables
     theme-name

     `(hl-todo-keyword-faces '(("TODO" . ,war)
                               ("NOTE" . ,yellow)
                               ("FIXME" . ,war))))
    ))

(deftheme cjh-theme "My theme")
(cjh-create-theme 'cjh-theme)
(provide-theme 'cjh-theme)
(enable-theme 'cjh-theme)

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

;; TODO(chogan): Enable multiple consecutive uses
(defun cjh-end-of-word ()
  (interactive)
  (forward-word)
  (backward-char))

(defvar cjh-last-find-char nil)
(defvar cjh-last-end-of-find-point nil)
(defvar cjh-last-find-direction nil)

;; TODO(chogan): Don't require pressing enter
(defun cjh-find-forward ()
  (interactive)
  (let ((char (read-string "Find char: "))
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

;; TODO(chogan): Don't require pressing enter
(defun cjh-find-backward ()
  (interactive)
  (let ((char (read-string "Find char: "))
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

(defun cjh-quit-help ()
  (interactive)
  (cjh-normal-state)
  (quit-window))

(defun enable-cjh-mode ()
  (cjh-mode 1))

;;; Hooks
;; Should never be in normal state in the minibuffer
(add-hook 'minibuffer-setup-hook 'cjh-insert-state)
(add-hook 'minibuffer-exit-hook 'cjh-normal-state)
;; Comment out #if 0 blocks
(add-hook 'c-mode-common-hook 'cjh-c-mode-common-hook)
;; Don't indent when inside a namespace
(add-hook 'c++-mode-hook #'(lambda () (c-set-offset 'innamespace [0])))
;; Include underscores in word
(add-hook 'python-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'c-mode-common-hook #'(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'help-mode-hook 'cjh-set-help-cursor)
(add-hook 'help-mode-hook (lambda () (local-set-key "q" 'cjh-quit-help)))
;; (add-hook 'isearch-mode-end-hook (lambda () (setq cjh-isearch-state nil)))
(add-hook 'prog-mode-hook 'enable-cjh-mode)
(add-hook 'org-mode-hook 'enable-cjh-mode)
(add-hook 'messages-buffer-mode-hook 'enable-cjh-mode)

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
     (other . "linux")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
