(require 'package)

(custom-set-variables    
 '(package-selected-packages
   (quote
    (evil-nerd-commenter evil-surround evil nyan-mode magit-svn magit omnisharp org))))

(custom-set-variables 
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))))

(package-initialize)
