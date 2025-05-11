;; https://www.emacswiki.org/emacs/FullScreen
(load "~/crafted-emacs/modules/crafted-early-init-config")

(menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

;; https://www.emacswiki.org/emacs/FullScreen
(push '(fullscreen . maximized) default-frame-alist)

(load-theme 'modus-vivendi-tinted)
