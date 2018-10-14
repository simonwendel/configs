(package-initialize)

(custom-set-variables
;; fonts and colors and crap
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(blink-cursor-mode nil)
 '(custom-enabled-themes (quote (wombat)))

;; maximize screen space
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)

;; set up sources 
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))

 ;; list packages to move to new machine
 '(package-selected-packages (quote (org))))

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
