
(set-language-environment 'Japanese)

(prefer-coding-system 'utf-8)

;; path

(defun add-load-paths (paths)
  (if (null paths)
      '()
      (progn
            (let ((default-directory (car paths))) (normal-top-level-add-subdirs-to-load-path))
            (add-load-paths (cdr paths)))))

;(add-load-paths
;    '("~/.emacs.d/site-lisp" "/opt/local/share/emacs/site-lisp" "/opt/local/share/emacs/23.2/site-lisp"))

(add-load-paths '("~/.emacs.d/site-lisp" "/usr/share/emacs/site-lisp" "/usr/share/emacs23/site-lisp"))

;; key binding

(if (eq window-system 'x)
    (progn
     (define-key function-key-map [backspace] [8])
     (put 'backspace 'ascii-character 8)
    ))

(global-set-key "\C-h" 'backward-delete-char)

(global-set-key "\177" 'delete-char)

;; tab

(setq-default tab-width 4)

;; view

(menu-bar-mode 0)
(tool-bar-mode 0)

(global-linum-mode t)
;(setq linum-format "%d ")

(show-paren-mode 1)

(blink-cursor-mode t)

(if window-system
	(set-scroll-bar-mode nil))
(setq scroll-step 5)

;; color

(defun color-theme-pi8027 ()
  "pi8027's theme"
  (interactive)
  (color-theme-install
   '(color-theme-pi8027
     ((foreground-color . "white")
      (background-color . "black")
      (background-mode . dark))
     (default ((t (nil))))
     (region ((t (:foreground "yellow" :background "brown"))))
     (underline ((t (:foreground "yellow" :underline t))))
     (modeline ((t (:foreground "black" :background "wheat"))))
     (italic ((t (:foreground "dark red" :italic t))))
     (bold-italic ((t (:foreground "dark red" :bold t :italic t))))
     (font-lock-comment-face ((t (:foreground "Firebrick"))))
     (bold ((t (:bold)))))))

(require 'color-theme)
(color-theme-initialize)
(color-theme-pi8027)

;; font

(when (eq window-system 'ns)
	  (set-face-attribute 'default nil :family "Monaco" :height 100)
	  (set-fontset-font "fontset-default" 'japanese-jisx0208 '("Osaka" . "iso10646-1"))
	  (set-fontset-font "fontset-default" 'katakana-jisx0201 '("Osaka" . "iso10646-1"))
	  (setq face-font-rescale-alist
			'(("^-apple-hiragino.*" . 1.2)
			  (".*osaka-bold.*" . 1.2)
			  (".*osaka-medium.*" . 1.2)
			  (".*courier-bold-.*-mac-roman" . 1.0)
			  (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
			  (".*monaco-bold-.*-mac-roman" . 0.9)
			  ("-cdac$" . 1.3))))

;; ddskk

(autoload 'skk-mode "skk" nil t)
(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key "\C-xj" 'skk-auto-fill-mode)
(setq skk-large-jisyo "~/.skk/SKK-JISYO.L")

(add-hook 'isearch-mode-hook
          #'(lambda ()
              (when (and (boundp 'skk-mode)
                         skk-mode
                         skk-isearch-mode-enable)
                (skk-isearch-mode-setup))))
(add-hook 'isearch-mode-end-hook
          #'(lambda ()
              (when (and (featurep 'skk-isearch)
                         skk-isearch-mode-enable)
                (skk-isearch-mode-cleanup))))

;; yatex

(setq auto-mode-alist (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)

(setq YaTeX-kanji-code 4)

;; haskell-mode

(load "haskell-site-file")

;; sdic-mode

(global-set-key "\C-cw" 'sdic-describe-word)

(global-set-key "\C-c\C-w" 'sdic-describe-word-at-point)

(setq sdic-window-height 10
	  sdic-disable-select-window t)

