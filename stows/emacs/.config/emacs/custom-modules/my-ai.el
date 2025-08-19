;; -*- lexical-binding: t; -*-

;; Author: Witoslaw Koczewski <wi@koczewski.de>

(defun my/ai-prompt-file (filename)
  (expand-file-name (concat "/p/orga/ai/prompts/" filename) user-emacs-directory))

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

;;; gptel
;; https://github.com/karthink/gptel

(use-package gptel
  :defer t
  
  :config
  
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
  (defun my/gptel-scratch ()
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
  
  (my/gptel-make-tools)
   
  :bind
  ("M-<return>" . my/gptel-menu)
  ("C-c a i c" . my/gptel-scratch)
  
  )

;;; elysium
;; https://github.com/lanceberge/elysium

(use-package elysium)



;;; aidermacs
;; https://github.com/MatthewZMD/aidermacs

(use-package aidermacs
  ;; :bind (("C-c a" . aidermacs-transient-menu))
  
  ;; :config
                                        
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "deepseek-coder"))

;;; provide
(provide 'my-ai)
