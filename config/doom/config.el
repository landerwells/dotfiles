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
(setq scroll-margin 8)

(map! :n "C-d" (cmd! (evil-scroll-down nil) (recenter))
      :n "C-u" (cmd! (evil-scroll-up nil) (recenter)))

(map! :leader
      :desc "Toggle Olivetti mode" "z" #'olivetti-mode)

(map! :leader
      :desc "Search org-roam notes"
      "n r s" #'consult-ripgrep)

(defun jethro/tag-new-node-as-draft ()
  (org-roam-tag-add '("draft")))
(add-hook 'org-roam-capture-new-node-hook #'jethro/tag-new-node-as-draft)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-roam-directory (file-truename "~/org/roam/"))
(setq org-roam-db-location (expand-file-name "org-roam.db" org-roam-directory))
(setq org-cite-global-bibliography '("~/org/roam/reference/reference.bib"))
(setq org-startup-with-inline-images t)

(after! org
  (add-to-list 'org-modules 'org-habit t))

(setq org-agenda-files 
      (directory-files (expand-file-name "agenda" org-roam-directory) t "\\.org$"))

;; I might want to put 

(setq org-capture-templates
      `(("i" "Inbox" entry  (file "roam/agenda/todo.org")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))))

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
         :unnarrored t)))

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

;; Not even sure if this function is useful or needed
(defun lw/org-no-fill-in-src-block ()
  "Prevent auto-fill inside Org src blocks."
  (let ((element (org-element-at-point)))
    (and
     (eq (org-element-type element) 'src-block)
     t)))

(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode 1)
            (setq fill-column 80)
            (auto-fill-mode 1)
            (add-hook 'fill-nobreak-predicate #'lw/org-no-fill-in-src-block nil t)))

(after! which-key
  (setq which-key-idle-delay 0
        which-key-idle-secondary-delay 0))

(after! elfeed
  (setq rmh-elfeed-org-files (list "~/dotfiles/config/doom/elfeed.org")))

(setq-default tab-width 2)
(setq-default evil-shift-width 2)
(setq indent-tabs-mode nil) ;; Ensure spaces are used instead of tabs


;; Issues with vterm,
;; 1. The vterm toggle, and vterm/here are not the same terminal. I would like for them to be the same instance of the shell since they are both getting opened in the same project.
;; 2. Getting keybindings from every other thing interfereing with it allk

(after! projectile
  (setq projectile-project-search-path '(("~") ("~/Developer"))))
