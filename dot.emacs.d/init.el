
(set-language-environment 'Japanese)

(prefer-coding-system 'utf-8)

;; path

(defun add-load-paths (paths)
  (if (null paths)
      '()
      (progn
            (let ((default-directory (car paths))) (normal-top-level-add-subdirs-to-load-path))
            (add-load-paths (cdr paths)))))

(add-load-paths
    '("~/.emacs.d/site-lisp" "/opt/local/share/emacs/site-lisp" "/opt/local/share/emacs/23.2/site-lisp"))

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

(menu-bar-mode nil)
(global-linum-mode t)

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

;; haskell-mode

(require 'haskell-site-file)
