
(set-language-environment 'Japanese)

(prefer-coding-system 'utf-8)

;; key binding

(if (eq window-system 'x)
	(progn
		(define-key function-key-map [backspace] [8])
		(put 'backspace 'ascii-character 8)
	))

(global-set-key "\C-h" 'backward-delete-char)

(global-set-key "\177" 'delete-char)

;; color

(if window-system (progn
	(add-to-list 'default-frame-alist '(foreground-color . "white"))
	(add-to-list 'default-frame-alist '(background-color . "black"))
	(add-to-list 'default-frame-alist '(cursor-color . "SlateBlue2"))
	(add-to-list 'default-frame-alist '(mouse-color . "SlateBlue2"))
	(set-face-foreground 'modeline "black")
	(set-face-background 'modeline "gray100")
	(set-face-background 'region "LightSteelBlue1")
	(set-face-foreground 'mode-line-inactive "gray30")
	(set-face-background 'mode-line-inactive "gray85")
))
