;;;; language, encoding

(package-initialize)

(set-language-environment 'English)

(prefer-coding-system 'utf-8)

;;;; path

(defun add-load-paths (paths)
  (dolist (default-directory paths)
    (if (file-exists-p default-directory)
	(progn (add-to-list 'load-path default-directory)
           (normal-top-level-add-subdirs-to-load-path)))))

(add-load-paths
    '("~/.emacs.d/site-lisp"
      "~/.emacs.d/lisp"
      "/usr/share/emacs/site-lisp"
      "/usr/local/share/emacs/site-lisp"
      "/opt/local/share/emacs/site-lisp"))

;;;; key binding

; assign both C-h and BackSpace to BackSpace (backward-delete-char)
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

(global-display-line-numbers-mode t)
(blink-cursor-mode t)
(show-paren-mode 1)

(setq-default display-fill-column-indicator-column 80)
(setq-default display-fill-column-indicator-character ?\ )
; TODO: The stipple should be calculated from window-font-width (cf. indent-bars)
(set-face-attribute 'fill-column-indicator nil :stipple '(16 1 "\x0\x10"))

(if window-system (set-scroll-bar-mode nil))
(setq scroll-step 5)

(setq inhibit-startup-message t)

(setq mode-line-format
      '(""
        skk-modeline-input-mode
        "%e "
        mode-line-front-space
        mode-line-mule-info
        mode-line-client
        mode-line-modified
        mode-line-remote
        mode-line-frame-identification
        mode-line-buffer-identification
        "   "
        mode-line-position
        (vc-mode vc-mode)
        "  "
        mode-line-modes
        mode-line-misc-info
        mode-line-end-spaces))

;;;; clipborad

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-selection-value)

;;;; color

(defun color-theme-kazuhiko ()
  "theme"
  (interactive)
  (color-theme-install
   '(color-theme-kazuhiko
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
     (bold ((t (:bold))))
     (font-lock-comment-face ((t (:foreground "salmon"))))
     (font-lock-function-name-face ((t (:foreground "#FFFFFF")))))))

(require 'color-theme)
(color-theme-initialize)
(color-theme-kazuhiko)

;;;; font

(cond
  ((eq window-system 'x)
   (set-face-attribute 'default nil :family "Inconsolata" :height 150)
   (set-fontset-font "fontset-default" 'japanese-jisx0208 (font-spec :family "M PLUS 1 Code"))
   (set-fontset-font "fontset-default" 'japanese-jisx0212 (font-spec :family "M PLUS 1 Code"))
   (setq face-font-rescale-alist
     '((".*inconsolata.*" . 1)
       (".*M PLUS.*" . 0.89))))
  ((eq window-system 'ns)
   (set-face-attribute 'default nil :family "Monaco" :height 150)
   (set-fontset-font "fontset-default" 'japanese-jisx0208 (font-spec :family "Osaka"))
   (set-fontset-font "fontset-default" 'japanese-jisx0212 (font-spec :family "Osaka"))
   (setq face-font-rescale-alist
     '(("^-apple-hiragino.*" . 1.2)
       (".*osaka-bold.*" . 1.2)
       (".*osaka-medium.*" . 1.2)
       (".*courier-bold-.*-mac-roman" . 1.0)
       (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
       (".*monaco-bold-.*-mac-roman" . 0.9)
       ("-cdac$" . 1.3)))))

;;;; auto-complete.el

;(require 'auto-complete)
;(require 'auto-complete-config)
;(ac-config-default)
;;(global-auto-complete-mode t)
;(setq auto-complete-keys 'ac-keys-backquote-backslash)
;
;(setq ac-auto-show-menu 0)
;(setq ac-quick-help-delay 0)
;(setq ac-auto-start 0)

;;;; ddskk

(autoload 'skk-mode "skk" nil t)
(global-set-key "\C-x\C-j" 'skk-mode)
(global-set-key "\C-xj" 'skk-auto-fill-mode)
;(setq skk-server-host "localhost")
;(setq skk-server-port 1178)
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

;;;; YaTeX

(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(add-to-list 'auto-mode-alist '("\\.tex$" . yatex-mode))
(setq YaTeX-kanji-code 4)
(setq YaTeX-fill-column 100)

(add-hook 'yatex-mode-hook '(lambda ()
			      (electric-indent-local-mode nil)))

;;;; Gauche

(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
(setq scheme-program-name "gosh -i")

;;;; OCaml (Tuareg, OCP, Merlin)

(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
(opam-setup-tuareg)
(opam-setup-ocp-indent)
(opam-setup-merlin)

;;;; Coq Proof General

(require 'proof-site "~/.emacs.d/lisp/PG/generic/proof-site")

(setq proof-splash-enable nil)
(setq proof-three-window-enable t)
(setq proof-three-window-mode-policy 'smart)
(setq split-width-threshold 180)
(setq coq-accept-proof-using-suggestion 'never)

(add-to-list 'auto-mode-alist '("\\.v" . coq-mode))
(load-file "~/.emacs.d/pg-ssr.el")

(setq coq-prog-args nil)
(setq coq-load-path-include-current t)

(add-hook 'coq-mode-hook '(lambda () (electric-indent-local-mode nil)))
(add-hook 'coq-mode-hook 'display-fill-column-indicator-mode)

(setq coq-indent-proofstart 0)
(setq coq-indent-modulestart 0)

;;;; gdb

(setq gdb-many-windows t)
(setq gdb-use-separate-io-buffer t)

;;;; magit

(autoload 'magit "magit" nil t)

;;;; sdic-mode

(autoload 'sdic-describe-word "sdic" nil t)

(global-set-key "\C-cw" 'sdic-describe-word)

(global-set-key "\C-c\C-w" 'sdic-describe-word-at-point)

(setq sdic-window-height 15
	  sdic-disable-select-window t)

(setq sdic-eiwa-dictionary-list '((sdicf-client "/usr/share/dict/eijiro.sdic")))
(setq sdic-waei-dictionary-list '((sdicf-client "/usr/share/dict/waeijiro.sdic")))

;;;; ielm

;(defun ielm-auto-complete ()
;  "Enables `auto-complete' support in \\[ielm]."
;  (setq ac-sources '(ac-source-functions
;                     ac-source-variables
;                     ac-source-features
;                     ac-source-symbols
;                     ac-source-words-in-same-mode-buffers))
;  (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
;  (auto-complete-mode 1))
;(add-hook 'ielm-mode-hook 'ielm-auto-complete)

;;;; local configuration

(if (file-exists-p "~/.emacs.d/init.local.el")
    (load-file "~/.emacs.d/init.local.el"))
