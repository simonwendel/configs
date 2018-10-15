(package-initialize)

;; set shell to Git bash
(setq explicit-shell-file-name "C:/Program Files/Git/bin/bash.exe")
(setq shell-file-name "bash")
(setq explicit-bash.exe-args '("--login" "-i"))
(setenv "SHELL" shell-file-name)
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
(prefer-coding-system 'utf-8)

;; magit stuff ->
(setq vc-handled-backends nil) ;; turn off vc

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-set-key [f1] 'shell)

(setq magit-refresh-status-buffer nil) ;; should be faster ops
(setq global-magit-file-mode 1) ;; adds commands to file visiting buffers

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (wombat)))
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(nyan-mode t)
 '(package-selected-packages (quote (org nyan-mode magit-svn)))
 '(tool-bar-mode nil))

'(package-archives
  (quote
   (("gnu" . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/"))))
'(package-selected-packages (quote (nyan-mode magit-svn magit org)))

;; Visual Studio default font
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#242424" :foreground "#f6f3e8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "outline" :family "Consolas"))))
 '(cursor ((t (:background "magenta")))))

;; flashes the modeline instead of bell for notifications
(setq ring-bell-function
      (lambda () (let ((orig-fg (face-foreground 'mode-line)))
		   (set-face-foreground 'mode-line "#F2804F")
		   (run-with-idle-timer 0.1 nil
					(lambda (fg) (set-face-foreground 'mode-line fg)) orig-fg))))
