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
(map! :leader
      :desc "Toggle Olivetti mode" "z" #'olivetti-mode
      :desc "Search org-roam notes" "n r s" #'consult-ripgrep)

;;; Org Mode & Org-Roam

;; Set directories before org loads
(setq org-directory "~/org/"
      org-roam-directory (file-truename "~/org/roam/")
      org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory)
      org-cite-global-bibliography '("~/org/roam/reference/reference.bib")
      org-startup-with-inline-images t
      org-agenda-files (directory-files (expand-file-name "agenda" org-roam-directory) t "\\.org$"))

(after! org
  ;; Enable org modules
  (add-to-list 'org-modules 'org-habit t)
  (add-to-list 'org-modules 'org-protocol t)

  ;; Capture templates
  (setq org-capture-templates
        '(("i" "Inbox" entry (file "roam/agenda/todo.org")
           "* TODO %?\n/Entered on/ %U")
          ("c" "org-protocol-capture" entry (file "roam/agenda/todo.org")
           "* TODO [[%:link][%:description]]\n\n%i" :immediate-finish t)))

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
                              "#+title: ${title}\n#+date: %<%B %d, %Y %I:%M %p>\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new (file+head "reference/${title}.org"
                              "#+title: ${title}\n#+date: %<%B %d, %Y %I:%M %p>\n")
           :immediate-finish t
           :unnarrowed t)))

  ;; Tag new nodes as draft
  (add-hook 'org-roam-capture-new-node-hook
            (lambda () (org-roam-tag-add '("draft")))))

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
  (setq projectile-project-search-path '(("~") ("~/Developer"))))

;; Racket/Geiser configuration
(setq geiser-racket-extra-keywords '("require" "sicp"))

;; EPUB reader
(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-text-width 80))

;;; Private Configuration

;; Load private org-gcal credentials if available
(let ((private-config (expand-file-name "private/org-gcal-credentials.el" doom-private-dir)))
  (when (file-exists-p private-config)
    (load private-config)))
