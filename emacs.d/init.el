
;;;; language, encoding

(set-language-environment 'Japanese)

(prefer-coding-system 'utf-8)

;;;; path

(defun add-load-paths (paths)
  (dolist (default-directory paths)
    (if (file-exists-p default-directory)
	(normal-top-level-add-subdirs-to-load-path))))

(add-load-paths
    '("~/.emacs.d/site-lisp"
      "/usr/share/emacs/site-lisp"
      "/usr/share/emacs23/site-lisp"
      "/opt/local/share/emacs/site-lisp"
      "/opt/local/share/emacs/23.2/site-lisp"))

;;;; key binding

(global-set-key "\C-h" 'backward-delete-char)
(global-set-key "\177" 'backward-delete-char)

;;;; tab

(setq tab-width 4)
(setq indent-tabs-mode nil)

;;;; backup

(setq backup-inhibited t)

;;;; view

(setq frame-title-format "GNU Emacs")

(menu-bar-mode 0)
(tool-bar-mode 0)

(global-linum-mode t)
;; (if (eq window-system nil)
;;   (setq linum-format "%d ")
;;   (setq linum-format "%d"))

(show-paren-mode 1)

(blink-cursor-mode t)

(if window-system
	(set-scroll-bar-mode nil))
(setq scroll-step 5)

;;;; color

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

;;;; font

(defun my-font-settings (scale)
  (cond ((eq window-system 'x)
	 (set-face-attribute 'default nil :family "Terminus" :height (floor (* scale 80)))
	 (set-fontset-font "fontset-default" 'katakana-jisx0201 '("M+ 2m" . "iso10646-1"))
	 (set-fontset-font "fontset-default" 'japanese-jisx0208 '("M+ 2m" . "iso10646-1"))
	 (set-fontset-font "fontset-default" 'japanese-jisx0212 '("M+ 2m" . "iso10646-1")))
	((eq window-system 'ns)
	 (set-face-attribute 'default nil :family "Monaco" :height (floor (* scale 100)))
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
		 ("-cdac$" . 1.3))))))

(my-font-settings 1)

;;;; auto-complete.el

(require 'auto-complete)
(global-auto-complete-mode t)

(setq ac-auto-show-menu 0)
(setq ac-quick-help-delay 0)
(setq ac-auto-start 0)

;;;; ddskk

(autoload 'skk-mode "skk" nil t)
(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key "\C-xj" 'skk-auto-fill-mode)
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

;;;; YaTeX

(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(add-to-list 'auto-mode-alist '("\\.tex$" . yatex-mode))
(setq YaTeX-kanji-code 4)
(setq YaTeX-fill-column 100)

;;;; Gauche

(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
(setq scheme-program-name "gosh -i")

;;;; Haskell (ghc-mod)



;;;; OCaml (TypeRex)

(autoload 'typerex-mode "typerex" "Major mode for editing OCaml code" t)
(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . typerex-mode))
(add-to-list 'interpreter-mode-alist '("ocamlrun" . typerex-mode))
(add-to-list 'interpreter-mode-alist '("ocaml" . typerex-mode))

(setq ocp-server-command "/usr/local/bin/ocp-wizard")

(setq ocp-theme "tuareg")

(setq typerex-let-always-indent nil)
(setq typerex-with-indent 0)
(setq typerex-function-indent 0)
(setq typerex-fun-indent 0)
(setq typerex-if-then-else-indent 0)

(setq ocp-auto-complete t)

;;;; agda-mode

(require 'agda2)

(setq agda2-include-dirs '("" "/home/pi8027/lib/agda/"))

(add-hook 'agda2-mode-hook
    (function (lambda () (add-to-list 'agda2-ghci-options "+RTS -M2G -K1G -RTS"))))

;;;; proof general

(autoload 'proof-site "coq-mode" "Coq proof assistant on Emacs" t)
(setq coq-load-path '("."))

;;;; gdb

(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)

;;;; sdic-mode

(autoload 'sdic-describe-word "sdic" nil t)

(global-set-key "\C-cw" 'sdic-describe-word)

(global-set-key "\C-c\C-w" 'sdic-describe-word-at-point)

(setq sdic-window-height 15
	  sdic-disable-select-window t)

(setq sdic-eiwa-dictionary-list '((sdicf-client "/usr/share/dict/eijiro.sdic")))
(setq sdic-waei-dictionary-list '((sdicf-client "/usr/share/dict/waeijiro.sdic")))

;;;; ielm

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

;;;; w3m

(autoload 'w3m "w3m" "Interface for w3m on Emacs" t)
(setq w3m-home-page "http://google.com/")
