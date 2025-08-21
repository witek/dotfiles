;;; early-init.el --- Emacs early initialization  -*- lexical-binding: t; -*-

;; Author: Witoslaw Koczewski <wi@koczewski.de>


(require 'package)

;;; Setup Emacs Lisp Package Archives (ELPAs)

(when (version< emacs-version "28")
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(add-to-list 'package-archives '("stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;;; Configure ELPA priorities
;; Prefer GNU sources and stable versions before development versions from MELPA.
(customize-set-variable 'package-archive-priorities
                        '(("gnu"    . 99)   ; prefer GNU packages
                          ("nongnu" . 80)   ; use non-gnu packages if
                                        ; not found in GNU elpa
                          ("stable" . 70)   ; prefer "released" versions
                                        ; from melpa
                          ("melpa"  . 0)))  ; if all else fails, get it
                                        ; from melpa

;;; Initialize package system
(add-hook 'before-init-hook 'package-initialize)


;;; my custom shit


(menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

;; https://www.emacswiki.org/emacs/FullScreen
(push '(fullscreen . maximized) default-frame-alist)

(load-theme 'modus-vivendi-tinted)
