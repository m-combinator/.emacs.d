(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
(package-initialize)


(setq my-packages '(cider
                    buffer-move
                    persistent-soft
                    ido-better-flex
                    ido-load-library
                    ido-select-window
                    ido-ubiquitous
                    ido-yes-or-no
                    paredit
                    pretty-lambdada
                    rainbow-delimiters
                    bubbleberry-theme
                    color-theme
                    assemblage-theme
                    zenburn-theme
                    flycheck
                    slime
                    jedi
                    auto-complete
                    paradox))

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))


;; default buffer is *scratch*
(setq initial-buffer-choice t)

(require 'buffer-move)

;;Some global key bindings to make things easier for me
(global-set-key (kbd "C-c p") 'list-packages)
(global-set-key (kbd "C-c r") 'package-refresh-contents)
(global-set-key (kbd "C-c <left>") 'buf-move-left)
(global-set-key (kbd "C-c <right>") 'buf-move-right)
(global-set-key (kbd "C-c <up>") 'buf-move-up)
(global-set-key (kbd "C-c <down>") 'buf-move-down)
(global-set-key (kbd "C-x w") 'whitespace-mode)


;; I don't know how to do multiline comments in emacs, boo
(defmacro comment (&rest code)
  "comment macro similar to clojure's comment macro"
  nil)


;;Some hooks

(show-paren-mode t)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (local-set-key (kbd "RET") 'newline-and-indent)
                                  (pretty-lambda-mode t)
                                  (paredit-mode t)))

(add-hook 'eval-expression-minibuffer-setup-hook  (lambda ()
                                                    (pretty-lambda-mode t)
                                                    (paredit-mode t)))

(add-hook 'ielm-mode-hook  (lambda ()
                             (pretty-lambda-mode t)
                             (paredit-mode t)))

(add-hook 'lisp-mode-hook  (lambda ()
                             (slime-mode t)
                             (local-set-key (kbd "RET") 'newline-and-indent)
                             (pretty-lambda-mode t)
                             (paredit-mode t)))

(add-hook 'lisp-interaction-mode-hook (lambda ()
                                        (local-set-key (kbd "RET") 'newline-and-indent)
                                        (pretty-lambda-mode t)
                                        (paredit-mode t)))

(add-hook 'scheme-mode-hook (lambda ()
                              (local-set-key (kbd "RET") 'newline-and-indent)
                              (pretty-lambda-mode t)
                              (paredit-mode t)))


                                        ;(require 'flymake-python-pyflakes)

                                        ;(add-hook 'after-init-hook #'global-flycheck-mode)
                                        ;(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
                                        ;(setq flymake-python-pyflakes-executable "flake8")
                                        ;(setq flymake-mode 1)
(require 'auto-complete)
(global-auto-complete-mode t)

(require 'jedi)
(add-hook 'python-mode-hook (lambda ()
                              (jedi:setup)
                              (flycheck-mode t)
                              (local-set-key (kbd "RET") 'newline-and-indent)
                              (pretty-lambda-mode t)))

(setq jedi:complete-on-dot t)


(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))


(comment
 (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
 (add-hook 'cider-mode-hook
           (lambda ()
             (local-set-key (kbd "RET") 'newline-and-indent)
             (set-syntax-table clojure-mode-syntax-table)
             (setq lisp-indent-function 'clojure-indent-function)))

 (setq cider-repl-popup-stacktraces t)
 (setq cider-repl-print-length 100)
 (add-hook 'cider-repl-mode-hook 'paredit-mode))

(eval-after-load 'clojure-mode
  '(progn
     (setq clojure-mode-use-backtracking-indent t)
     (add-hook 'clojure-mode-hook
               (lambda ()
                 (show-paren-mode t)
                 (paredit-mode t)
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

;;disable vc-git
(setq vc-handled-backends ())
                                        ;(eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))
(add-to-list 'custom-theme-load-path "/home/lazylambda/blackboard-theme")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (wheatgrass)))
 '(custom-safe-themes
   (quote
    ("f641bdb1b534a06baa5e05ffdb5039fb265fde2764fbfd9a90b0d23b75f3936b" "f0d8af755039aa25cd0792ace9002ba885fd14ac8e8807388ab00ec84c9497d7" "617219c11282b84761477059b9339da78ce392c974d9308535ee4ec8c0770bee" "1177fe4645eb8db34ee151ce45518e47cc4595c3e72c55dc07df03ab353ad132" "7dd0db710296c4cec57c39068bfffa63861bf919fb6be1971012ca42346a417f" "f3278046d89cd5bc16fbe006a9fdec1d20b4466f12d5e80ee7a92dd4a34ff886" "e83c94a6bfab82536cef63610ec58d08dfddd27752d860763055daf58d028aad" "dd4db38519d2ad7eb9e2f30bc03fba61a7af49a185edfd44e020aa5345e3dca7" "180adb18379d7720859b39124cb6a79b4225d28cef4bfcf4ae2702b199a274c8" default)))
 '(fci-rule-color "#383838")
 '(linum-format "%3i")
 '(paradox-github-token t)
 '(powerline-color1 "#3d3d68")
 '(powerline-color2 "#292945")
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(load-theme 'wheatgrass t)


(ido-mode t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)

;;turn on line numbers
(global-linum-mode t)
(setq column-number-mode t)
;;those look nasty, so let's remove em
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

;; require newline endings after editing files
(setq require-final-newline t)

(setq-default indent-tabs-mode nil)
(setq tab-width 4)
