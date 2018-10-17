;; need some bootstrapping to make sure all packages are available
(load-file "~/.emacs.d/packages.el")

;; here is where the magic happens
(org-babel-load-file "~/.emacs.d/configuration.org")

