;;; init.el --- Witek's Emacs Configuration  -*- lexical-binding: t; -*-

(setq user-full-name "Witoslaw Koczewski")
(setq user-mail-address "wi@koczewski.de")

;; my-modules directory
(add-to-list 'load-path (expand-file-name "my-modules" user-emacs-directory))

(require 'my-emacs-defaults)
(require 'my-utils)
(require 'my-theme)
(require 'my-modal)

;;; Keys

(my/set-custom-key "b d" 'kill-current-buffer)
(my/set-custom-key "b j" 'bookmark-jump)
(my/set-custom-key "b J" 'bookmark-jump-other-frame)
  
(my/set-custom-key "d K" 'describe-keymap)
(my/set-custom-key "d m" 'describe-mode)

(my/set-custom-key "e q" 'save-buffers-kill-terminal)
(my/set-custom-key "e Q" 'save-buffers-kill-emacs)
(my/set-custom-key "e e" 'eval-expression)
(my/set-custom-key "e l" 'eval-last-sexp)
(my/set-custom-key "e b" 'eval-buffer)
(my/set-custom-key "e f" 'eval-defun)
(my/set-custom-key "e r" 'restart-emacs)

(my/set-custom-key "f S" 'save-buffer)
(my/set-custom-key "f f" 'find-file)

(my/set-custom-key "w w" 'other-window)
(my/set-custom-key "w l" 'window-left)
(my/set-custom-key "w r" 'window-right)
(my/set-custom-key "w n" 'next-window-any-frame)
(my/set-custom-key "w p" 'previous-window-any-frame)
(my/set-custom-key "w /" 'split-window-horizontally)
(my/set-custom-key "w -" 'split-window-vertically)
(my/set-custom-key "w d" 'delete-window)
(my/set-custom-key "w m" 'delete-other-windows)

(my/set-custom-key "s q" 'query-replace)

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)

  :config
  (setq recentf-max-saved-items 100)
  (setq recentf-max-menu-items 25)
  (setq recentf-save-file-modes nil)
  (setq recentf-keep nil)
  (setq recentf-auto-cleanup nil)
  (setq recentf-initialize-file-name-history nil)
  (setq recentf-filename-handlers nil)
  (setq recentf-show-file-shortcuts-flag nil)
  (recentf-mode 1)
  )

(use-package outline
  
  :config

  (my/set-custom-key "o t" 'outline-toggle-children)
  (my/set-custom-key "o f" 'outline-hide-other)
  (my/set-custom-key "o a" 'outline-show-all)

  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
  (add-hook 'clojure-mode-hook #'outline-minor-mode)
  )

(use-package outline-minor-faces
  :ensure t
  :after outline

  :config
  (add-hook 'outline-minor-mode-hook
            #'outline-minor-faces-mode))

(use-package project
  :demand t
  
  :config
  (my/set-custom-key "p p" 'project-switch-project)
  (my/set-custom-key "p f" 'project-find-file)

  )

;;; org

(use-package org
  :ensure t
  :bind (
         :map org-src-mode-map
         ("C-c C-c" . org-edit-src-exit)     
         )
  
  :config
  (setq org-agenda-files '("~/org/inbox.org" "/home/witek/org/gtd.org"))
  (setq org-directory "~/org/")
  (setq org-archive-location "~/org/archive.org")
  (setq org-default-notes-file "~/org/inbox.org")
  (setq org-return-follows-link t)
  (setq org-blank-before-new-entry t)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-reverse-note-order t)

  ;; Return or left-click with mouse follows link
  (setq org-return-follows-link t)
  (setq org-mouse-1-follows-link t)

  ;; Display links as the description provided
  (setq org-link-descriptive t)

  ;; Visually indent org-mode files to a given header level
  ;; (add-hook 'org-mode-hook #'org-indent-mode)

  ;; Hide markup markers
  (setq org-hide-emphasis-markers nil)
  (when (locate-library "org-appear")
    (add-hook 'org-mode-hook 'org-appear-mode))

  (add-hook 'org-mode-hook (lambda () (toggle-truncate-lines)))
  )

(use-package org-modern
  :ensure t
  :after org
  
  :config
  (global-org-modern-mode))

;; https://protesilaos.com/emacs/denote#h:5d16932d-4f7b-493d-8e6a-e5c396b15fd6
(use-package denote
  :ensure t
  :defer t

  :hook (dired-mode . denote-dired-mode)

  :config
  (setq denote-directory (expand-file-name "/p/orga/denote/"))
  (setq denote-save-buffer-after-creation nil)
  (setq denote-known-keywords '("emacs" "dev" "home" "happygast" "fbst"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  ;; (setq denote-file-type 'markdown-toml)
  (setq denote-file-type 'org)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-no-confirm t)
  (setq denote-date-prompt-use-org-read-date t)
  (setq denote-backlinks-show-context t)
  (denote-rename-buffer-mode 1)
  (add-hook 'context-menu-functions #'denote-context-menu)
  )

(use-package denote-journal
  :ensure t
  )

;;; lsp-mode

(defun my/lsp-rename ()
  (interactive)
  (call-interactively 'save-some-buffers)
  (call-interactively 'lsp-rename)
  (save-some-buffers t)
  )

(defun my/lsp-organize-imports ()
  (interactive)
  (call-interactively 'save-some-buffers)
  (call-interactively 'lsp-organize-imports)
  (save-some-buffers t)
  (call-interactively 'lsp-organize-imports)
  (save-some-buffers t)
  )

(defun my/lsp-clojure-clean-ns ()
  (interactive)
  (call-interactively 'save-some-buffers)
  (call-interactively 'lsp-clojure-clean-ns)
  (save-some-buffers t)
  )

(use-package lsp-mode
  :ensure t
  :demand t
  :hook ((clojure-mode . lsp))
  :bind
  (:map witek-context-key-map
        ("r" . 'my/lsp-rename))

  :config

  (setq lsp-file-watch-threshold 5000)
  ;; (setq lsp-keymap-prefix ", r")

  ;; We don't want to get asked about project root
  (setq lsp-auto-guess-root t)

  ;; Because we don't use company
  (setq lsp-completion-provider :none)

  (setq lsp-enable-indentation t)
  (setq lsp-enable-snippet t)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-log-io nil)

  ;; Don't underline on errors/warnings
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-headerline-breadcrumb-icons-enable nil)

  (setq lsp-lens-enable t)
  ;; (setq lsp-lens-place-position 'above-line)

  ;; (custom-set-faces
  ;;  '(lsp-headerline-breadcrumb-separator-face ((t :inherit shadow :height 0.7)))
  ;;  '(lsp-headerline-breadcrumb-path-face ((t :inherit font-lock-string-face :height 0.7)))
  ;;  '(lsp-headerline-breadcrumb-project-prefix-face ((t :inherit font-lock-string-face :weight bold :height 0.7)))
  ;;  '(lsp-headerline-breadcrumb-symbols-face ((t :inherit font-lock-doc-face :weight bold :height 0.7)))
  ;;  '(lsp-lens-face ((t (:inherit lsp-details-face :height 0.7)))))

  ;; (setq lsp-headerline-breadcrumb-path-face '((t :inherit font-lock-string-face :height 0.5)))

  (lsp-semantic-tokens--warn-about-deprecated-setting)

  )

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  ;; :demand t
  ;; :commands lsp-ui-mode


  :config

  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions nil)

  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-use-webkit t)
  
  ;; (custom-set-faces
  ;;  '(lsp-headerline-breadcrumb-path-face ((t :inherit font-lock-string-face :height 0.7)))
  ;;  '(lsp-headerline-breadcrumb-project-prefix-face ((t :inherit font-lock-string-face :weight bold :height 0.7)))
  ;;  '(lsp-headerline-breadcrumb-separator-face ((t :inherit shadow :height 0.7)))
  ;;  '(lsp-headerline-breadcrumb-symbols-face ((t :inherit font-lock-doc-face :weight bold :height 0.7)))
  ;;  '(lsp-lens-face ((t (:inherit lsp-details-face :height 0.7))))
  ;;  )

  )

(use-package lsp-treemacs
  :ensure t
  :defer t
  :commands lsp-treemacs-errors-list)

(use-package consult-lsp
  :ensure t
  :defer t
  :after (consul lsp-mode)
  )

;;; AI

(defun my/ai-prompt-file (filename)
  (expand-file-name (concat "/p/orga/ai/prompts/" filename) user-emacs-directory))

;; https://github.com/karthink/gptel
(defun my/ai-gptel-load-directive-from-markdown (file)
  "Load a gptel directive from a markdown FILE.
Returns a cons of (name . directive) where name is derived from filename
and directive is the content of the file."
  (condition-case err
      (let ((max-specpdl-size (* 10 max-specpdl-size)) ; Increase recursion limit
            (max-lisp-eval-depth (* 10 max-lisp-eval-depth))
            (large-file-warning-threshold nil) ; Disable large file warning
            (gc-cons-threshold (* 100 1024 1024))) ; 100MB for GC threshold
        (with-temp-buffer
          ;; Temporarily increase buffer size limit for this operation
          (let ((enable-local-variables nil)
                (buffer-read-only nil)
                (buffer-file-name nil)
                (max-mini-window-height 0.5))
            (insert-file-contents file)
            (let* ((filename (file-name-nondirectory file))
                   (name (intern (car (split-string filename "\\.md"))))
                   (content (buffer-substring-no-properties
                             (point-min)
                             (point-max))))
              (cons name (string-trim content))))))
    (error
     (message "Error loading directive from %s: %s"
              file (error-message-string err))
     nil)))

(defun my/ai-gptel-load-all-markdown-directives (directory)
  "Load all markdown files from DIRECTORY as gptel directives.
Returns a list of cons cells (name . directive) for each .md file."
  (when (file-directory-p directory)
    (let ((markdown-files (directory-files directory t "\\.md$")))
      (delq nil
            (mapcar #'my/ai-gptel-load-directive-from-markdown markdown-files)))))

(defun my/gptel-make-tools ()

  (gptel-make-tool
   :name "emacs_eval"
   :description "Evaluate Emacs Lisp Code inside the running Emacs"
   :function (lambda (code)
               (read "(progn 1 2)")
               (eval
                (read
                 (concat "(progn " code ")"))))
   :args (list '(:name "elisp_code"
                       :type "string"
                       :description "The Emacs Lisp Code to evaluate"))
   :confirm t
   :include nil
   :category "emacs")

  (gptel-make-tool
   :name "fetch"
   :description "Fetch and read the contents of a URL"
   :function (lambda (url)
               (with-current-buffer (url-retrieve-synchronously url)
                 (goto-char (point-min)) (forward-paragraph)
                 (let ((dom (libxml-parse-html-region (point) (point-max))))
                   (run-at-time 0 nil #'kill-buffer (current-buffer))
                   (with-temp-buffer
                     (shr-insert-document dom)
                     (buffer-substring-no-properties (point-min) (point-max))))))
   :args (list '(:name "url"
                       :type "string"
                       :description "The URL to read"))
   :category "web")

  ;; (gptel-make-tool
  ;;  :function
  ;;  (lambda ()
  ;;    (if-let* ((proj (project-current))
  ;;              (root (project-root proj)))
  ;;        (let ((root-path (expand-file-name root)))
  ;;          (format "Project root directory: %s\nDirectory exists: %s\nIs directory: %s"
  ;;                  root-path
  ;;                  (file-exists-p root-path)
  ;;                  (file-directory-p root-path)))
  ;;      "No project found in the current context."))
  ;;  :name "get_project_root"
  ;;  :description "Get the root directory of the current project. This is useful for understanding the project structure and performing operations relative to the project root."
  ;;  :args nil
  ;;  :category "project")

  (gptel-make-tool
   :name "read_file"
   :description "Read and display the contents of a file"
   :function (lambda (filepath)
	       (with-temp-buffer
	         (insert-file-contents (expand-file-name filepath))
	         (buffer-string)))
   :args (list '(:name "filepath"
	               :type "string"
	               :description "Path to the file to read.  Supports relative paths and ~."))
   :category "filesystem")

  (gptel-make-tool
   :name "list_project_files"
   :description "List programming files in the current project directory. Use this function to understand which files you want to read so you can better understand the request from the user."
   :function
   (lambda (&optional file-regex)
     (if-let* ((proj (project-current))
               (root (project-root proj))
               (default-regex "\\.\\(el\\|clj\\|cljs\\|cljc\\|js\\|jsx\\|ts\\|tsx\\|rb\\|py\\|go\\|rs\\|cpp\\|c\\|h\\|hpp\\|java\\|php\\)$")
               (regex (or file-regex default-regex))
               (files (project-files proj)))
         (let ((matching-files
                (cl-remove-if-not
                 (lambda (file)
                   (string-match-p regex (file-relative-name file root)))
                 files)))
           (concat "Project root: " (abbreviate-file-name root) "\n"
                   "Files:\n"
                   (mapconcat
                    (lambda (file)
                      (concat "- " (file-relative-name file root)))
                    matching-files
                    "\n")))
       "No project found or no matching files."))
   :args (list '(:name "file_regex"
                       :type "string"
                       :description "Optional regex pattern to filter files (e.g., \"\\.py$\" for Python files). If not provided, lists common programming files."))
   :category "project")
  
  )


(use-package gptel
  :ensure t
  :defer t
        
  :config
  (require 'gptel-transient)
        
  (setq gptel-model `deepseek-chat)
  (setq gptel-temperature 0.7)
  (setq gptel-window-select t)
  (setq gptel-window-side 'right)
  (setq gptel-window-width 80)

  (setq gptel-default-mode 'org-mode)
  (setq gptel-org-branching-context nil)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "# user: ")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "# llm:\n\n")

  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
        
  (setq gptel-api-key (auth-source-pass-get 'secret "openai-emacs"))
        
  (setq gptel-backend
        (gptel-make-anthropic "Claude"
          :stream t
          :key (auth-source-pass-get 'secret "anthropic-emacs")))
  ;; (setq gptel-model `claude-3-haiku-20240307)

  (setq gptel-backend
        (gptel-make-openai "DeepSeek"
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :stream t
          :key (auth-source-pass-get 'secret "deepseek-emacs")
          :models '(deepseek-chat deepseek-coder)))
        
  (setq gptel-directives
        (let ((markdown-directives (my/ai-gptel-load-all-markdown-directives (expand-file-name "prompts" user-emacs-directory))))
          `(
            ,@markdown-directives
            )))

  ;; my/gptel-menu command
  (defun my/gptel-menu ()
    (interactive)
    (meow-insert-exit)
    (gptel-menu)
    )

  ;; my/gptel-scratch command
  (setq my/gptel-scratch-buffer-name "*gptel-scratch*")
  (defun my/ai-chat ()
    (interactive)
    (let ((buffer-name (concat
                        "*gptel-chat "
                        (format-time-string "%Y-%m-%d %H:%M:%S")
                        "*")))
      (progn
        (gptel buffer-name)
        (switch-to-buffer buffer-name)
        )
      )
    ;; (if (not (get-buffer my/gptel-scratch-buffer-name))
    ;;     (gptel my/gptel-scratch-buffer-name))
    ;; (switch-to-buffer my/gptel-scratch-buffer-name)
    )

  (defun my/ai-emacs-do ()
    "Execute the users request in emacs"
    (interactive)

    ;; tool
    (let ((emacs-eval-tool
           (gptel-make-tool
            :name "emacs_eval"
            :function (lambda (code)
                        (condition-case err
                            (let ((result (eval (read code))))
                              (format "Evaluation result: %S" result))
                          (error (format "Error: %s" (error-message-string err)))))
            :description "Evaluate Emacs Lisp code and return the result"
            :args '((:name "code"
                           :type "string"
                           :description "Emacs Lisp code to evaluate")))))
              
      ;; Add tool to gptel-tools and enable tool use
      (setq-local gptel-tools (list emacs-eval-tool))
      (setq-local gptel-use-tools t)
      )

    ;; system prompt
    (setq-local gptel--system-message "You are a helpful assistant living inside Emacs.
        Use the emacs_eval tool to fullfill the request of the user.")
          
    ;; user prompt and execution
    ;; (let ((prompt (gptel--read-minibuffer-prompt)))
    ;;   (call-interactively #'gptel-menu))

    (gptel--suffix-send '("m" "e"))
    )
        
  (my/gptel-make-tools)
         
  :bind
  ("M-<return>" . my/gptel-menu)
  ("C-c a i c" . my/ai-chat)
  ("C-c a i d" . my/ai-emacs-do)
        
  )

;; https://github.com/lanceberge/elysium
(use-package elysium
  :ensure t
  :defer t
  )

;; https://github.com/MatthewZMD/aidermacs
(use-package aidermacs
  :ensure t
  :defer t
  ;; :bind (("C-c a" . aidermacs-transient-menu))
  
  :init
  (setq aidermacs-use-architect-mode t)
  (setq aidermacs-default-model "deepseek-coder"))




(require 'my-commands)
(require 'my-basics)
(require 'my-extras)

(add-hook 'after-init-hook
          (lambda ()
            (find-file "~/.dotfiles/stows/emacs/.config/emacs/init.el")))
