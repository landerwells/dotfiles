;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; General Settings
(setq doom-theme 'doom-gruvbox
      display-line-numbers-type 'relative
      scroll-margin 0)

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

;; Org-cite: use CSL (citeproc) with IEEE style for all export backends,
;; including ox-hugo. See https://ox-hugo.scripter.co/doc/org-cite-citations/
(after! oc
  (setq org-cite-csl-styles-dir (expand-file-name "csl" doom-user-dir)
        org-cite-export-processors '((t csl "ieee.csl"))))

(defun lw/org-auto-print-bibliography (backend)
  "Append `#+print_bibliography:' when exporting a buffer with citations.
Runs on the transient export copy, so the source note is untouched.
Only acts when BACKEND derives from hugo and the buffer has at least one
citation but no existing print_bibliography keyword."
  (when (org-export-derived-backend-p backend 'hugo)
    (save-excursion
      (goto-char (point-min))
      (when (and (re-search-forward org-element-citation-prefix-re nil t)
                 (not (save-excursion
                        (goto-char (point-min))
                        (re-search-forward "^[ \t]*#\\+print_bibliography:" nil t))))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "\n#+print_bibliography:\n")))))

(add-hook 'org-export-before-processing-functions #'lw/org-auto-print-bibliography)

(after! org
  ;; Enable org modules
  (add-to-list 'org-modules 'org-habit t)
  (add-to-list 'org-modules 'org-protocol t)

  ;; Org Babel support for SICP Scheme blocks.
  (require 'ob-scheme)

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

(set-file-template! "/org/roam/.+\\.org$" :ignore t)

(after! org-roam
  (require 'org-roam-protocol)
  ;; Capture templates
  (setq org-roam-capture-templates
        '(("f" "fleeting" plain "%?"
           :if-new (file+head "fleeting/${title}.org"
                              "#+title: ${title}\n#+date: %<%B %d, %Y %I:%M %p>\n")
           :immediate-finish t
           :unnarrowed t)
          ("m" "main" plain "%?"
           :if-new (file+head "main/${title}.org"
                              "#+title: ${title}\n#+date: %<%B %d, %Y %I:%M %p>\n#+filetags: :draft:\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new (file+head "reference/${title}.org"
                              "#+title: ${title}\n#+date: %<%B %d, %Y %I:%M %p>\n")
           :immediate-finish t
           :unnarrowed t)))

  ;; Web clipping via org-roam-protocol
  (setq org-roam-capture-ref-templates
        '(("w" "web clip" plain
           "%?"
           :if-new (file+head "reference/${slug}.org"
                              "#+title: ${title}\n#+roam_key: ${ref}\n#+date: %<%B %d, %Y %I:%M %p>\n#+filetags: :web:\n")
           :unnarrowed t))))

;; Org protocol needs to be loaded early
(require 'org-protocol)

;;; Helper Functions

(defun lw/org-no-fill-in-src-block ()
  "Prevent auto-fill inside Org src blocks."
  (let ((element (org-element-at-point)))
    (and (eq (org-element-type element) 'src-block) t)))

;;; Package Configuration

(after! doom-modeline
  (defvar-local lw/buffer-word-count nil
    "Cached word count for modeline display.")

  (defun lw/update-word-count (&rest _)
    "Update cached word count for the current buffer."
    (setq lw/buffer-word-count (count-words (point-min) (point-max))))

  (run-with-idle-timer 2 t #'lw/update-word-count)
  (add-hook 'after-save-hook #'lw/update-word-count)
  (add-hook 'find-file-hook #'lw/update-word-count)

  (doom-modeline-def-segment buffer-word-count
    "Display the word count of the current buffer."
    (when (and (doom-modeline--active) lw/buffer-word-count)
      (concat
       (doom-modeline-spc)
       (propertize (format "%dW" lw/buffer-word-count)
                   'face 'doom-modeline-info)
       (doom-modeline-spc))))

  (doom-modeline-def-modeline 'main
    '(eldoc bar workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding buffer-word-count major-mode process vcs check time)))

(after! which-key
  (setq which-key-idle-delay 0
        which-key-idle-secondary-delay 0))

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

(use-package folgezett
  :load-path "~/Developer/folgezett.el"
  :after org-roam
  :init
  ;; Automatically prepend the folgezettel ID to captured note filenames.
  ;; `folgezett-capture-keys' restricts this to the "main" org-roam template.
  (setq folgezett-capture-keys '("m")
        folgezett-db-link-parent t
        folgezett-include-id-in-filename t)
  :config
  (folgezett-setup))

(map! :leader
      (:prefix-map ("n z" . "folgezettel")
       :desc "Assign ID"        "a" #'folgezett-assign-id
       :desc "Goto parent"      "p" #'folgezett-goto-parent
       :desc "List children"    "c" #'folgezett-list-children
       :desc "Show tree"        "t" #'folgezett-show-tree
       :desc "Reparent"         "r" #'folgezett-reparent
       :desc "Reparent subtree" "R" #'folgezett-reparent-subtree))

;; I think this is truly the route I want to go down. Push straight to
;; the server. All revisions will happen
;;
;; -- site: Any of the structured files that I would typically have in my website
;; -- cards: Anything that comes from my main zettelkasten notes
;; -- images? I think that should definitely work out
;;
;; What kind of automation do I want with this?
;;
;; I think there should be some sort of staging area? That would make the most sense.
;; I don't just want to accidentally push a bunch of stuff. All I need to do is update
;; htdocs/ when we get some new information
;;
;; Global HTML export options
(setq org-export-with-section-numbers nil
      org-export-with-toc nil
      org-export-with-author nil
      org-export-with-date nil
      org-export-with-timestamps nil
      org-html-postamble nil
      org-export-with-title nil)

(setq org-html-head-include-default-style nil
      org-html-head-include-scripts nil
      org-html-head "<link rel=\"stylesheet\" href=\"/main.css\">")

(setq org-publish-project-alist
      `(("cards"
         :base-directory "~/org/roam/main"
         :base-extension "org"
         :publishing-directory ,(expand-file-name "output/cards" org-roam-directory)
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :html-preamble lw/html-preamble)

        ("images"
         :base-directory "~/images/"
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory "/ssh:user@host:~/html/images/"
         :publishing-function org-publish-attachment)

        ("website"
         :base-directory "~/org/roam/website"
         :base-extension "org"
         :publishing-directory ,(expand-file-name "output" org-roam-directory)
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :html-preamble lw/html-preamble
         :recursive t)

        ("landerwells.com"
         :components ("cards" "images" "website"))))


(defun lw/html-preamble (_plist)
  (with-temp-buffer
    (insert-file-contents "~/dotfiles/config/doom/header.html")
    (buffer-string)))
