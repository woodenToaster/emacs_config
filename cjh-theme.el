(deftheme cjh-theme "Based on Spacemacs dark theme.")

(defun cjh-create-theme (theme-name)
  (let* ((class '((class color) (min-colors 89)))
	 (true-color-p (lambda () (or
				   (display-graphic-p)
				   (= (tty-display-color-cells) 16777216))))
        ;;                                   GUI     Terminal
        ;; (act1          (if (true-color-p) "#222226" "#121212"))
        (act2          (if (true-color-p) "#5d4d7a" "#444444"))
        (base          (if (true-color-p) "#b2b2b2" "#b2b2b2"))
        (base-dim      (if (true-color-p) "#686868" "#585858"))
        (bg1           (if (true-color-p) "#292b2e" "#262626"))
        (bg2           (if (true-color-p) "#212026" "#1c1c1c"))
        (bg3           (if (true-color-p) "#100a14" "#121212"))
        ;; (bg4           (if (true-color-p) "#0a0814" "#080808"))
        (border        (if (true-color-p) "#5d4d7a" "#111111"))
        ;; (cblk          (if (true-color-p) "#cbc1d5" "#b2b2b2"))
        ;; (cblk-bg       (if (true-color-p) "#2f2b33" "#262626"))
        ;; (cblk-ln       (if (true-color-p) "#827591" "#af5faf"))
        ;; (cblk-ln-bg    (if (true-color-p) "#373040" "#333333"))
        (cursor        (if (true-color-p) "#ffcd48" "#d0d0d0"))
        (const         (if (true-color-p) "#a45bad" "#d75fd7"))
        (comment       (if (true-color-p) "#2aa1ae" "#008787"))
        ;; (comment-light (if (true-color-p) "#2aa1ae" "#008787"))
        ;; (comment-bg    (if (true-color-p) "#292e34" "#262626"))
        (comp          (if (true-color-p) "#c56ec3" "#d75fd7"))
        (err           (if (true-color-p) "#e0211d" "#e0211d"))
        (func          (if (true-color-p) "#bc6ec5" "#d75fd7"))
        ;; (head1         (if (true-color-p) "#4f97d7" "#268bd2"))
        ;; (head1-bg      (if (true-color-p) "#293239" "#262626"))
        ;; (head2         (if (true-color-p) "#2d9574" "#2aa198"))
        ;; (head2-bg      (if (true-color-p) "#293235" "#262626"))
        ;; (head3         (if (true-color-p) "#67b11d" "#67b11d"))
        ;; (head3-bg      (if (true-color-p) "#293235" "#262626"))
        ;; (head4         (if (true-color-p) "#b1951d" "#875f00"))
        ;; (head4-bg      (if (true-color-p) "#32322c" "#262626"))
        (highlight     (if (true-color-p) "#444155" "#444444"))
        ;; (highlight-dim (if (true-color-p) "#3b314d" "#444444"))
        (keyword       (if (true-color-p) "#4f97d7" "#268bd2"))
        ;; (lnum          (if (true-color-p) "#44505c" "#444444"))
        (mat           (if (true-color-p) "#86dc2f" "#86dc2f"))
        (meta          (if (true-color-p) "#9f8766" "#af875f"))
        (str           (if (true-color-p) "#2d9574" "#2aa198"))
        (suc           (if (true-color-p) "#86dc2f" "#86dc2f"))
        ;; (ttip          (if (true-color-p) "#9a9aba" "#888888"))
        (ttip-sl       (if (true-color-p) "#5e5079" "#333333"))
        ;; (ttip-bg       (if (true-color-p) "#34323e" "#444444"))
        (type          (if (true-color-p) "#ce537a" "#df005f"))
        (var           (if (true-color-p) "#7590db" "#8787d7"))
        ;; ;;(war           (if (true-color-p) "#dc752f" "#dc752f"))
        (war           (if (true-color-p) "#ffe3eb" "#dc752f"))

        ;; colors
        ;; (aqua          (if (true-color-p) "#2d9574" "#2aa198"))
        ;; (aqua-bg       (if (true-color-p) "#293235" "#262626"))
        ;; (green         (if (true-color-p) "#67b11d" "#67b11d"))
        (green-bg      (if (true-color-p) "#293235" "#262626"))
        (green-bg-s    (if (true-color-p) "#29422d" "#262626"))
        ;; (cyan          (if (true-color-p) "#28def0" "#00ffff"))
        ;; (red           (if (true-color-p) "#f2241f" "#d70000"))
        ;; (red-bg        (if (true-color-p) "#3c2a2c" "#262626"))
        ;; (red-bg-s      (if (true-color-p) "#512e31" "#262626"))
        ;; (blue          (if (true-color-p) "#4f97d7" "#268bd2"))
        ;; (blue-bg       (if (true-color-p) "#293239" "#262626"))
        ;; (blue-bg-s     (if (true-color-p) "#2d4252" "#262626"))
        ;; (magenta       (if (true-color-p) "#a31db1" "#af00df"))
        (yellow        (if (true-color-p) "#b1951d" "#875f00"))
     ;; (yellow-bg     (if (true-color-p) "#32322c" "#262626"))
     )

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
    `(trailing-whitespace ((,class (:background "#7af5f5"))))
    `(secondary-selection ((,class (:background ,bg3))))
    `(shadow ((,class (:foreground ,base-dim))))
    `(success ((,class (:foreground ,suc))))
    `(tooltip ((,class (:background ,ttip-sl :foreground ,base :bold nil :italic nil :underline nil))))
    `(vertical-border ((,class (:foreground ,border))))
    `(warning ((,class (:foreground ,war))))

    `(highlight-symbol-face ((,class (:background ,bg2))))

    ;; show-paren
    `(show-paren-match ((,class (:foreground ,mat :inherit bold))))
    `(show-paren-match-expression ((,class (:background ,green-bg-s))))
    `(show-paren-mismatch ((,class (:foreground ,err :inherit bold)))))))

(cjh-create-theme 'cjh-theme)
(provide-theme 'cjh-theme)
