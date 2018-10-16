(package-initialize)

;; always start main frame maximized
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/Todo.org"))

;; sunrise commander stuff
;; remember to clone into .emacs.d/local repo from
;; https://github.com/escherdragon/sunrise-commander.git
(add-to-list 'load-path "~/.emacs.d/local/sunrise-commander")
(require 'sunrise-commander)
(require 'sunrise-x-buttons)
(require 'sunrise-x-modeline)
(add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode))
(global-set-key "\C-x\C-d" 'sunrise-cd)

;; set shell to Git bash
(setq explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe")
(setq shell-file-name "bash")
(setq explicit-bash.exe-args '("--login" "-i"))
(setenv "SHELL" shell-file-name)
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
(prefer-coding-system 'utf-8)
(global-set-key [f1] 'shell)

;; magit stuff ->
(setq vc-handled-backends nil) ;; turn off vc
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(setq magit-refresh-status-buffer nil) ;; should be faster ops
(setq global-magit-file-mode 1) ;; adds commands to file visiting buffers

(custom-set-variables

 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (wombat)))
 '(nyan-mode t)

 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)

'(package-archives
  (quote
   (("gnu" . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/"))))

'(package-selected-packages (quote (nyan-mode magit-svn magit org))))

;; Visual Studio default font
(custom-set-faces
 '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "outline" :family "Consolas"))))
 '(cursor ((t (:background "magenta")))))

;; flashes the modeline instead of bell for notifications
(setq ring-bell-function
      (lambda () (let ((orig-fg (face-foreground 'mode-line)))
		   (set-face-foreground 'mode-line "#F2804F")
		   (run-with-idle-timer 0.1 nil
					(lambda (fg) (set-face-foreground 'mode-line fg)) orig-fg))))
