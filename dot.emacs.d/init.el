
(set-language-environment 'Japanese)

(prefer-coding-system 'utf-8)

;; path

(defun add-load-paths (paths)
  (if (null paths)
      nil
      (progn
       (let* ((head (car paths))
              (default-directory head))
             (if (file-exists-p head)
                 (normal-top-level-add-subdirs-to-load-path)))
       (add-load-paths (cdr paths)))))

(add-load-paths
    '("~/.emacs.d/site-lisp"
      "/usr/share/emacs/site-lisp"
      "/usr/share/emacs23/site-lisp"
      "/opt/local/share/emacs/site-lisp"
      "/opt/local/share/emacs/23.2/site-lisp"))

;; key binding

(if (eq window-system 'x)
    (progn
     (define-key function-key-map [backspace] [8])
     (put 'backspace 'ascii-character 8)
    ))

(global-set-key "\C-h" 'backward-delete-char)

(global-set-key "\177" 'backward-delete-char)

;; tab

(setq-default tab-width 4)

;; view

(setq frame-title-format "GNU Emacs")

(menu-bar-mode 0)
(tool-bar-mode 0)

(global-linum-mode t)
;(setq linum-format "%d ")

(show-paren-mode 1)

(blink-cursor-mode t)

(if window-system
	(set-scroll-bar-mode nil))
(setq scroll-step 5)

;; backup

(setq backup-inhibited t)

;; viper-mode

;(setq viper-mode t)
;(require 'viper)

;; color

(defun color-theme-pi8027 ()
  "pi8027's theme"
  (interactive)
  (color-theme-install
   '(color-theme-pi8027
     ((background-mode . dark)
      (background-color . "black")
      (foreground-color . "white")
      (border-color . "white")
      (cursor-color . "red"))
     (default ((t (nil))))
     (region ((t (:foreground "yellow" :background "brown"))))
     (underline ((t (:foreground "yellow" :underline t))))
     (modeline ((t (:foreground "black" :background "wheat"))))
     (italic ((t (:foreground "dark red" :italic t))))
     (bold-italic ((t (:foreground "dark red" :bold t :italic t))))
     (font-lock-comment-face ((t (:foreground "Firebrick"))))
     (bold ((t (:bold))))
     (font-lock-function-name-face ((t (:foreground "#FFFFFF")))))))

(require 'color-theme)
(color-theme-initialize)
(color-theme-pi8027)

;; font

(cond ((eq window-system 'ns)
	   (progn
		 (set-face-attribute 'default nil :family "Monaco" :height 100)
		 (set-fontset-font "fontset-default" 'katakana-jisx0201 '("Osaka" . "iso10646-1"))
		 (set-fontset-font "fontset-default" 'japanese-jisx0208 '("Osaka" . "iso10646-1"))
		 (set-fontset-font "fontset-default" 'japanese-jisx0212 '("Osaka" . "iso10646-1"))
		 (setq face-font-rescale-alist
			   '(("^-apple-hiragino.*" . 1.2)
				 (".*osaka-bold.*" . 1.2)
				 (".*osaka-medium.*" . 1.2)
				 (".*courier-bold-.*-mac-roman" . 1.0)
				 (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
				 (".*monaco-bold-.*-mac-roman" . 0.9)
				 ("-cdac$" . 1.3)))))
	  ((eq window-system 'x)
	   (progn
		 (set-face-attribute 'default nil :family "Terminus" :height 80)
		 (set-fontset-font "fontset-default" 'katakana-jisx0201 '("M+ 2m" . "iso10646-1"))
		 (set-fontset-font "fontset-default" 'japanese-jisx0208 '("M+ 2m" . "iso10646-1"))
		 (set-fontset-font "fontset-default" 'japanese-jisx0212 '("M+ 2m" . "iso10646-1")))))

;; auto-complete.el

(require 'auto-complete)
(global-auto-complete-mode t)

;; ddskk

(autoload 'skk-mode "skk" nil t)
(global-set-key "\C-x\C-j" 'skk-mode)
;(global-set-key "\C-xj" 'skk-auto-fill-mode)
(setq skk-server-host "localhost")
(setq skk-server-port 1178)
;(setq skk-large-jisyo "~/.skk/SKK-JISYO.L")

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

(setq YaTeX-fill-column 100)

;; gauche

(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

;; haskell-mode

(load "haskell-site-file")

;; agda-mode

(load "agda2-mode")

(setq agda2-include-dirs '("" "/home/pi8027/lib/agda/"))

(add-hook 'agda2-mode-hook
    (function (lambda () (add-to-list 'agda2-ghci-options "+RTS -M2G -K1G -RTS"))))

;; proof general

(load "proof-site")

;; sdic-mode

(autoload 'sdic "sdic" t nil)

(global-set-key "\C-cw" 'sdic-describe-word)

(global-set-key "\C-c\C-w" 'sdic-describe-word-at-point)

(setq sdic-window-height 15
	  sdic-disable-select-window t)

(setq sdic-eiwa-dictionary-list '((sdicf-client "/usr/share/dict/eijiro.sdic")))
(setq sdic-waei-dictionary-list '((sdicf-client "/usr/share/dict/waeijiro.sdic")))

;; ielm

(defun ielm-auto-complete ()
  "Enables `auto-complete' support in \\[ielm]."
  (setq ac-sources '(ac-source-functions
                     ac-source-variables
                     ac-source-features
                     ac-source-symbols
                     ac-source-words-in-same-mode-buffers))
  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
  (auto-complete-mode 1))
(add-hook 'ielm-mode-hook 'ielm-auto-complete)

;; w3m

(require 'w3m-load)
