;; Based on Crafted Emacs and davivil's config.


;; https://github.com/daviwil/dotfiles/blob/master/Emacs.org

(require 'crafted-defaults)    ; Sensible default settings for Emacs
(require 'crafted-ui)          ; Better UI experience (modeline etc.)
(require 'crafted-editing)     ; Whitespace trimming, auto parens etc.
(require 'crafted-windows)     ; Window management configuration
(require 'crafted-completion)  ; selection framework based on `vertico`
;; (require 'crafted-speedbar)    ; built-in file-tree
(require 'crafted-lisp)
(require 'crafted-project)
(require 'crafted-compile)     ; Set up automatic compilation for some emacs-lisp files

(require 'witek-defaults)

;; (require 'witek-evil)
(require 'witek-meow)

;; (require 'witek-eglot)
(require 'witek-lsp)

(require 'witek-extras)

(require 'witek-org)
(require 'witek-theme)

(server-start)
