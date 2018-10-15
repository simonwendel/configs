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

 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (wombat)))
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-selected-packages (quote (magit org))))

;; Visual Studio default font
(custom-set-faces
 '(default ((t (:family "Leelawadee UI" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))

;; flashes the modeline instead of bell for notifications
(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line)))
          (set-face-foreground 'mode-line "#F2804F")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg) (set-face-foreground 'mode-line fg))
                               orig-fg))))
