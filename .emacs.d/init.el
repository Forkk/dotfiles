;;(require 'cask "/run/current-system/sw/share/emacs/site-lisp/cask.el")
(require 'cask)
(cask-initialize)

(require 'nix-mode)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq fill-column 80)

(desktop-save-mode 1)

;; (evil-mode 1)
;; (global-evil-surround-mode 1)

(global-undo-tree-mode 1)

(setq-default fill-column 80)

(yas-global-mode 1)
(add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))

(global-git-gutter+-mode 1)

(latex-preview-pane-enable)

;;;; git-gutter+

;; (global-set-key (kbd "C-c C-s C-s") 'git-gutter+-stage-hunks)
;; (global-set-key (kbd "C-c C-s C-S-s") 'git-gutter+-revert-hunks)
;; (global-set-key (kbd "C-c C-s C-e") 'git-gutter+-next-hunk)
;; (global-set-key (kbd "C-c C-s C-w") 'git-gutter+-previous-hunk)
;; (global-set-key (kbd "C-c C-s C-b") 'git-gutter+-stage-whole-buffer)
;; (global-set-key (kbd "C-c C-s C-c") 'git-gutter+-commit)

(eval-after-load 'git-gutter+
  '(progn
     ;;; Jump between hunks
     (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)

     ;;; Act on hunks
     (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
     ;; (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)
     ;; Stage hunk at point.
     ;; If region is active, stage all hunk lines within the region.
     (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
     (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
     (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
     (define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
     ;; (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer)
     ))

;; (global-set-key (kbd "C-c C-s C-") ')


;;;; Haskell mode
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;;;; Scala

(add-hook 'scala-mode-hook '(lambda ()
   ;; sbt-find-definitions is a command that tries to find (with grep)
   ;; the definition of the thing at point.
   (local-set-key (kbd "M-.") 'sbt-find-definitions)

   ;; use sbt-run-previous-command to re-compile your code after changes
   (local-set-key (kbd "C-x '") 'sbt-run-previous-command)
))


;;;; Window Management
(winner-mode 1)
(windmove-default-keybindings 'meta)

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)


;;;; IDO Options
(require 'ido)
(require 'smex)
(smex-initialize)
(ido-mode t)
(ido-vertical-mode 1)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; Old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;;; Nameses
(require 'desktop)
(require 'nameses)
(setq nameses-ido-mode t)
(global-set-key (kbd "C-x C-a")   'nameses-load)
(global-set-key (kbd "C-x a")     'nameses-prev)
(global-set-key (kbd "C-x C-S-a") 'nameses-save)


;;;; Company Mode
;; (add-hook 'after-init-hook
;; 	  (lambda () 
;; 	    (global-company-mode)
;; 	    (add-to-list 'company-backends 'company-ghc)
;; 	    ))

;;;; Misc Keys

;; Set the terminal escape key to C-x instead of C-c.
(add-hook 'term-mode-hook
	  (lambda ()
	    ;; C-x is the prefix command, rather than C-c
	    (term-set-escape-char ?\C-x)))

;; Escape to get out of things, but don't close windows.
(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;;;; Switch between Literate Haskell and Markdown

(add-hook 'literate-haskell-mode-hook
	  (lambda ()
	    (global-set-key (kbd "M-\\") 'markdown-mode)))

(add-hook 'markdown-mode-hook
	  (lambda ()
	    (global-set-key (kbd "M-\\") 'literate-haskell-mode)))

(add-hook 'after-init-hook #'global-flycheck-mode)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;;(eval-after-load 'flycheck
;;  '(require 'flycheck-hdevtools))


;; ;;;; Evil Mode

;; ;; Use C-b to switch buffers quickly.
;; (global-unset-key (kbd "C-b"))
;; (global-set-key (kbd "C-b") 'switch-to-buffer)
;; (define-key evil-normal-state-map (kbd "C-b") 'switch-to-buffer)

;; ;; Map ; to :
;; (define-key evil-normal-state-map ";" 'evil-ex)
;; (define-key evil-visual-state-map ";" 'evil-ex)

;; ;;;; Multiple Cursors Options
;; ;; In visual mode, use "g c" to put cursors on each line.
;; (define-key evil-normal-state-map (kbd "g c") 'mc/edit-lines)
;; (define-key evil-visual-state-map (kbd "g c") 'mc/edit-lines)

;; ;; Mode specific settings for Evil mode.
;; (evil-set-initial-state 'dired-mode 'emacs)
;; (evil-set-initial-state 'term-mode 'emacs)
;; (evil-set-initial-state 'shell-mode 'emacs)
;; (evil-set-initial-state 'help-mode 'emacs)
;; (evil-set-initial-state 'flycheck-error-list-mode 'emacs)


;;;; Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style
   (quote
    ((c-mode . "stroustrup")
     (c++-mode . "stroustrup")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(coffee-tab-width 4)
 '(column-number-mode t)
 '(desktop-files-not-to-save nil)
 '(haskell-indent-after-keywords
   (quote
    (("where" 2 0)
     ("of" 2)
     ("do" 2)
     ("mdo" 2)
     ("rec" 2)
     ("in" 2 0)
     ("{" 2)
     "if" "then" "else" "let")))
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines nil)
 '(haskell-stylish-on-save t)
 '(ido-vertical-define-keys (quote C-n-C-p-up-down-left-right))
 '(ido-vertical-mode t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(js-indent-level 2)
 '(preview-auto-cache-preamble t)
 '(preview-default-preamble
   (quote
    ("\\RequirePackage["
     ("," . preview-default-option-list)
     "]{preview}[2004/11/05]" "\\PreviewEnvironment{tabular}" "\\PreviewEnvironment{tabulary}" "\\PreviewEnvironment{itemize}" "\\PreviewEnvironment{enumerate}")))
 '(purescript-mode-hook
   (quote
    (turn-on-purescript-indent turn-on-purescript-indentation)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)


;;;; Shell Commands

(defun shell-command-on-buffer ()
  "Asks for a command and executes it in inferior shell with current buffer as input."
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   (read-shell-command "Shell command on buffer: ")))

(global-set-key (kbd "M-\"") 'shell-command-on-buffer)


(setq backup-directory-alist '(("." . "~/.emacs.bak")))
