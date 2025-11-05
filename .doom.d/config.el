;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)
(setq display-line-numbers-type `relative)
(setq company-minimum-prefix-length 1)
(setq scroll-margin 8)

(map! :n "C-d" (cmd! (evil-scroll-down nil) (recenter))
      :n "C-u" (cmd! (evil-scroll-up nil) (recenter)))

(map! :n "gj" 'evil-next-visual-line
      :n "gk" 'evil-previous-visual-line)

(map! :leader
      :desc "Toggle Olivetti mode" "z" #'olivetti-mode)

(map! :leader
      (:prefix ("o" . "open")
       :desc "Switch between header/source" "o" #'ff-find-other-file))

(map! :leader
      :desc "Search org-roam notes"
      "n r s" #'consult-ripgrep)

(defun tag-new-node-as-draft ()
  (let ((file-name (buffer-file-name)))
    (when (and file-name
               (string-match-p "/main/" file-name)) ; Only act on files in "main" folder
      (org-roam-tag-add '("draft")))))

(add-hook 'org-roam-capture-new-node-hook #'tag-new-node-as-draft)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-roam-directory (file-truename "~/org/roam/"))
(setq org-startup-with-inline-images t)

(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "main/${title}.org"
                            "#+title: ${title}\n#+date: %<%B %d, %Y %I:%M %p>\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new
         (file+head "reference/${title}.org"
                    "#+title: ${title}\n#+date: %<%B %d, %Y %I:%M %p>\n")
         :immediate-finish t
         :unnarrowed t)
        ("b" "blog" plain "%?"
         :if-new
         (file+head "blog/${title}.org"
                    "#+title: ${title}\n#+date: %<%B %d, %Y %I:%M %p>\n")
         :immediate-finish t
         :unnarrowed t)
        ("x" "index" plain "%?"
         :if-new (file+head "index/${slug}.org"
                            "#+title: ${title}\n#+date: %<%B %d, %Y %I:%M %p>\n")
         :immediate-finish t
         :unnarrowed t)))

;; (use-package! org-roam-ui
;;   :after org-roam ;; or :after org
;;   ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;   ;;         a hookable mode anymore, you're advised to pick something yourself
;;   ;;         if you don't care about startup time, use
;;   :hook (after-init . org-roam-ui-mode)
;;   :config
;;   (setq org-roam-ui-sync-theme t
;;         org-roam-ui-follow t
;;         org-roam-ui-update-on-save t
;;         org-roam-ui-open-on-start nil))

(use-package org-roam
  :ensure t
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(after! org
  (setq org-babel-load-languages
        '((scheme . t)
          (emacs-lisp . t)   ;; Enable Emacs Lisp execution
          (python . t))))    ;; Enable Python

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; create a function to disalbe line wrap on files other than markdown or org

(after! evil
  ;; Bind Ctrl+h, Ctrl+j, Ctrl+k, Ctrl+l directly for window navigation
  (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right))

;; https://orgmode.org/manual/Table-of-Contents.html
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)
(setq org-export-with-author nil)
(setq org-export-with-date nil)
(setq org-export-timestamp-file nil)


;; This works perfectly, now I need to make the index page work well, and get
;; a few more things working correctly. I would love to have this out by tomorrow,
;; but I might need a few more days to cook




;; I could have the index page function on its own? or write somewhat of a script
;; to handle everything? 

;; Function to find all org files with :publish: filetag


;; I'll come up with a script that will create the necessary portions I need and
;; put them at the bottom of my index.html


;; 


(defun lw/org-publish-flat-base-name (project file)
  "Publish every file directly into the project's publishing-directory."
  (file-name-nondirectory file))   ;; strip directories entirely



(defun lw/org-publish-files-in (directory)
  "Find all org files in DIRECTORY with :publish: filetag."
  (let ((files '()))
    (dolist (file (directory-files-recursively directory "\\.org$"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        ;; Look for #+filetags: line containing :publish:
        (when (re-search-forward "^#\\+filetags:.*:publish:" nil t)
          (push (file-relative-name file directory) files))))
    files))

(defun lw/update-blog-publish-list (&optional project-plist)
  "Update the :include list for the Blog project before publishing."
  (let* ((project (assoc "Blog" org-publish-project-alist))
         (base-dir (plist-get (cdr project) :base-directory))
         (files (lw/org-publish-files-in (expand-file-name base-dir))))
    ;; Update the :include property in the project plist
    (setf (cdr project)
          (plist-put (cdr project) :include files))))

(defun lw/org-sitemap-format-entry (entry style project)
  "Format sitemap ENTRY with date in 'Month Day, Year' format."
  (let* ((file (org-publish--expand-file-name entry project))
         (title (org-publish-find-title entry project))
         (date (org-publish-find-date entry project)))
    (if (= (length entry) 0)
        ""
      (format "%s - [[file:%s][%s]]"
              (if date
                  (format-time-string "%B %d, %Y" date)
                "No date")
              entry
              title))))

(setq org-publish-project-alist
      `(("Blog"
         :base-directory "~/org/roam/main/"
         :publishing-directory "~/Developer/landerwells.github.io/blog"
         :publishing-function org-html-publish-to-html
         :recursive t
         :section-numbers nil
         :with-toc nil
         :base-extension "org"
         :exclude ".*"
         :preparation-function lw/update-blog-publish-list
         :include ,(lw/org-publish-files-in "~/org/roam/main")
         :base-file-name-function lw/org-publish-flat-base-name
         :auto-sitemap t
         :sitemap-title "Blog Posts"
         :sitemap-filename "index.org"
         :sitemap-sort-files anti-chronologically
         :sitemap-format-entry lw/org-sitemap-format-entry)
        ("Website"
         :base-directory "~/org/roam/"
         :publishing-directory "~/Developer/landerwells.github.io/"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         :base-extension "org"
         :exclude ".*"
         :include ("index.org"))))

;; get rid of title on sitemap, need html preamble instead, apply custom css as well please
;;
;;I wish that the date and the post were on different lines, perhaps default css for posts?

;; Seems like I could use the index to make my braindump

(after! org
  (add-to-list 'org-modules 'org-habit t))

(setq org-agenda-files (directory-files-recursively org-roam-directory "\\.org$"))


(setq elfeed-feeds '(("https://www.sandordargo.com/feed.xml" cpp)))


                                        ; (setq org-html-table-default-attributes
                                        ;       '(:border "0" :rules "none" :cellspacing "0" :cellpadding "0" :frame "void"))

;; I want to move over any nvim binds to emacs, I would honestly prefer to start using this
;; There are so many things that I like about doom emacs that I just want to keep using it
;;
;; Feature that I certainly need is to be able to emulate my current workflow with tmux n
;; everything

;; How do I make images always visible when I am looking into a org mode file

(after! corfu
  (setq corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 1)
  (define-key corfu-map (kbd "C-y") #'corfu-complete))


(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/Developer/" "~/"))
  :config
  ;; I typically use this keymap prefix on macOS
                                        ; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ;; On Linux, however, I usually go with another one
                                        ; (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
                                        ; (global-set-key (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

