(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
(package-initialize)


(unless (package-installed-p 'cider)
  (package-install 'cider))


;; default buffer is *scratch*
(setq initial-buffer-choice t)


;;Some key bindings to make things easier for me
(global-set-key (kbd "C-x p") 'list-packages)
(global-set-key (kbd "C-c r") 'package-refresh-contents)


;; Set font
;(set-face-attribute 'default nil :font "Source Code Pro-13")

(defmacro comment (&rest code)
  "comment macro similar to clojure's comment macro"
  nil)


;;enable paredit mode for lisp modes

(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (progn
				    (local-set-key (kbd "RET") 'newline-and-indent)
				    (pretty-lambda-mode t)
				    (paredit-mode t))))

(add-hook 'eval-expression-minibuffer-setup-hook  (lambda ()
						    (progn
						      (pretty-lambda-mode t)
						      (paredit-mode t))))
(add-hook 'ielm-mode-hook  (lambda ()
			     (progn
			       (pretty-lambda-mode t)
			       (paredit-mode t))))

(add-hook 'lisp-mode-hook  (lambda ()
			     (progn
			       (pretty-lambda-mode t)
			       (paredit-mode t))))

(add-hook 'lisp-interaction-mode-hook (lambda ()
					(progn
					  (local-set-key (kbd "RET") 'newline-and-indent)
					  (pretty-lambda-mode t)
					  (paredit-mode t))))
	  
(add-hook 'scheme-mode-hook (lambda ()
			      (progn
				(pretty-lambda-mode t)
				(paredit-mode t))))


(comment
 (add-hook 'cider-repl-mode-hook (lambda ()
				   (progn
				     (pretty-lambda-mode t)
				     (paredit-mode t)))))

(comment
 (add-hook 'clojure-mode-hook (lambda ()
				(progn
				  (local-set-key (kbd "RET") 'newline-and-indent)
				  (paredit-mode t)))))


(add-hook 'lisp-mode-hook (lambda ()
			    (local-set-key (kbd "RET") 'newline-and-indent)))


(eval-after-load 'clojure-mode
  '(progn
     (setq clojure-mode-use-backtracking-indent t)
     (add-hook 'clojure-mode-hook
               (lambda ()
		 (local-set-key (kbd "RET") 'newline-and-indent)
                 (put-clojure-indent 'fact 'defun)
                 (put-clojure-indent 'prepend 'defun)
                 (put-clojure-indent 'when-short 'defun)))))


(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode
    `(("(\\(fn\\>\\)"
       (0 (progn (compose-region (match-beginning 1)
                                 (match-end 1)
                                 ,(make-char 'greek-iso8859-7 107))
                 nil))))))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook
          (lambda ()
            (set-syntax-table clojure-mode-syntax-table)
            (setq lisp-indent-function 'clojure-indent-function)))
(setq cider-repl-popup-stacktraces t)
(setq cider-repl-print-length 100)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(comment
 ;;hooks for clojure mode
 (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
 (add-hook 'cider-repl-mode-hook 'subword-mode)
 (setq cider-repl-pop-to-buffer-on-connect t)
 (setq cider-popup-stacktraces t)
 (setq cider-repl-popup-stacktraces t)
 (setq cider-auto-select-error-buffer t)
 (setq cider-repl-history-file "~/.emacs.d/cider-history")
 (setq cider-repl-wrap-history t)
 (add-hook 'clojure-mode-hook 'subword-mode))


;;set window to maximize
(add-hook 'after-init-hook (lambda () (w32-send-sys-command #xf030)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "180adb18379d7720859b39124cb6a79b4225d28cef4bfcf4ae2702b199a274c8" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(load-theme 'zenburn t)


(ido-mode t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)

(global-linum-mode t)
					;(rainbow-delimiters-mode t)

(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

(setq show-trailing-whitespace t)
(show-paren-mode t)


;;comment this out for now as I don't need it
(comment

 (defun toggle-transparency ()
   (interactive)
   (let ((param (cadr (frame-parameter nil 'alpha))))
     (if (and param (/= param 100))
	 (set-frame-parameter nil 'alpha '(100 100))
       (set-frame-parameter nil 'alpha '(85 50)))))

 (global-set-key (kbd "C-c t") 'toggle-transparency))
