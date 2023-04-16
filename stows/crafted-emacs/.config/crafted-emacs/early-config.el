(setq package-enable-at-startup nil)
(setq crafted-package-system 'straight)
(crafted-package-bootstrap crafted-package-system)

(straight-use-package 'use-package)

(setq inhibit-compacting-font-caches t)
(setq helm-ag-show-status-function nil)
