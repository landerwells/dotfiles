;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; General Settings
(setq doom-theme 'doom-gruvbox
      display-line-numbers-type 'relative
      scroll-margin 8)

;; Indentation
(setq-default tab-width 2
              evil-shift-width 2
              indent-tabs-mode nil)

;;; Keybindings

;; Scrolling with recentering
(map! :n "C-d" (cmd! (evil-scroll-down nil) (recenter))
      :n "C-u" (cmd! (evil-scroll-up nil) (recenter)))

;; Window navigation
(map! :n "C-h" #'evil-window-left
      :n "C-j" #'evil-window-down
      :n "C-k" #'evil-window-up
      :n "C-l" #'evil-window-right)

;; Leader keybindings
;; (map! :leader
;;                                         ; :desc "Toggle Olivetti mode" "z" #'olivetti-mode
;;       :desc "Search org-roam notes" "n r s" #'consult-ripgrep)

;;; Org Mode & Org-Roam

;; Set directories before org loads
(setq org-directory "~/org/"
      org-roam-directory (file-truename "~/org/roam/")
      org-cite-global-bibliography '("~/org/roam/reference/reference.bib")
      org-startup-with-inline-images t
      org-agenda-files (directory-files (expand-file-name "agenda" org-roam-directory) t "\\.org$"))

;; Set your bibliography file(s)
(setq citar-bibliography '("~/org/roam/reference/reference.bib"))

;; Set your PDF library and notes paths
(setq citar-library-paths '("~/Books")
      citar-notes-paths '("~/org/roam"))

(after! org
  ;; Enable org modules
  (add-to-list 'org-modules 'org-habit t)
  (add-to-list 'org-modules 'org-protocol t)

  ;; Capture templates
  (setq org-capture-templates
        '(("i" "Inbox" entry (file "roam/agenda/todo.org")
           "* TODO %?\n/Entered on/ %U")
          ("c" "org-protocol-capture" entry (file "roam/agenda/todo.org")
           "* TODO [[%:link][%:description]]\n\n%i" :immediate-finish t)
          ("w" "Weight" table-line (file+headline "roam/agenda/health.org" "Weight Log")
           "| %<%Y-%m-%d> | %^{Weight (kgs)} |" :immediate-finish t)))

  ;; Org mode hooks for auto-fill
  (add-hook 'org-mode-hook
            (lambda ()
              (visual-line-mode 1)
              (setq fill-column 80)
              (auto-fill-mode 1)
              (add-hook 'fill-nobreak-predicate #'lw/org-no-fill-in-src-block nil t))))

(after! org-roam
  (require 'org-roam-protocol)
  ;; Capture templates
  (setq org-roam-capture-templates
        '(("m" "main" plain
           "%?"
           :if-new (file+head "main/${title}.org"
                              "#+title: ${title}\n#+date: %<%B %d, %Y %I:%M %p>\n#+filetags: :draft:\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new (file+head "reference/${title}.org"
                              "#+title: ${title}\n#+date: %<%B %d, %Y %I:%M %p>\n")
           :immediate-finish t
           :unnarrowed t))))

;; Org protocol needs to be loaded early
(require 'org-protocol)

;;; Helper Functions

(defun lw/org-no-fill-in-src-block ()
  "Prevent auto-fill inside Org src blocks."
  (let ((element (org-element-at-point)))
    (and (eq (org-element-type element) 'src-block) t)))

;;; Package Configuration

(after! which-key
  (setq which-key-idle-delay 0
        which-key-idle-secondary-delay 0))

(after! elfeed
  (setq rmh-elfeed-org-files (list "~/dotfiles/config/doom/elfeed.org"))

  ;; Auto-update when opening elfeed
  (add-hook 'elfeed-search-mode-hook
            (lambda ()
              (unless (get-buffer "*elfeed-log*")
                (elfeed-update)))))

(after! projectile
  (setq projectile-project-search-path '(("~" . 1) ("~/Developer" . 1)))

  (add-hook 'projectile-after-switch-project-hook #'projectile-invalidate-cache)

  (defun lw/projectile-root-search-path-child (dir)
    "Return DIR as project root if it is a direct child of a search path."
    (cl-some (lambda (search)
               (let* ((base (if (consp search) (car search) search))
                      (expanded (file-truename (expand-file-name base))))
                 (when (equal (file-truename (expand-file-name ".." dir))
                              expanded)
                   dir)))
             projectile-project-search-path))

  (add-to-list 'projectile-project-root-functions
               #'lw/projectile-root-search-path-child t))

;; Racket/Geiser configuration
(setq geiser-racket-extra-keywords '("require" "sicp"))

;;; Private Configuration

;; Load private org-gcal credentials if available
(let ((private-config (expand-file-name "private/org-gcal-credentials.el" doom-private-dir)))
  (when (file-exists-p private-config)
    (load private-config)))

(after! gptel
  (setq gptel-backend
        (gptel-make-anthropic "Claude"
          :stream t
          :key (lambda ()
                 (string-trim
                  (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "private/anthropic-api-key" doom-private-dir))
                    (buffer-string))))))
  (setq gptel-model 'claude-sonnet-4-6)
  (setq gptel-default-mode 'org-mode))

; (use-package folgezett
;   :load-path "~/Developer/folgezett.el"
;   :after org-roam
;   :config
;   (setq folgezett-capture-keys '("m"))
;   (folgezett-setup))
;
; (map! :leader
;       (:prefix-map ("n z" . "folgezettel")
;        :desc "Assign ID"        "a" #'folgezett-assign-id
;        :desc "Goto parent"      "p" #'folgezett-goto-parent
;        :desc "List children"    "c" #'folgezett-list-children
;        :desc "Show tree"        "t" #'folgezett-show-tree
;        :desc "Reparent"         "r" #'folgezett-reparent
;        :desc "Reparent subtree" "R" #'folgezett-reparent-subtree))
