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
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-gruvbox)
;; (setq doom-theme 'almost-mono-cream)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type `relative)

;; (defun tag-new-node-as-draft ()
;;   (org-roam-tag-add '("draft")))
;; (add-hook 'org-roam-capture-new-node-hook #'tag-new-node-as-draft)

(defun tag-new-node-as-draft ()
  (let ((file-name (buffer-file-name)))
    (unless (string-match-p "/daily/" file-name) ; Skip files in the "daily" folder
      (org-roam-tag-add '("draft")))))

(add-hook 'org-roam-capture-new-node-hook #'tag-new-node-as-draft)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-roam-directory (file-truename "~/org/roam/"))

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
        ("x" "indexes" plain "%?"
         :if-new (file+head "indexes/${slug}.org"
                            "#+title: ${title}\n#+date: %<%B %d, %Y %I:%M %p>\n")
         :immediate-finish t
         :unnarrowed t)))

(use-package! org-roam
  ;; :ensure t
  :init
  ;; (setq org-roam-directory "~/org-roam/")
  ;; other Org-roam settings
  :config
  (with-eval-after-load 'ox-html
    (defun my/org-html--details (details contents info)
      "Export DETAILS block as a folded <details> element by default."
      (let ((summary (org-element-property :summary details)))
        (format "<details>%s%s</details>"
                (if summary (format "<summary>%s</summary>" summary) "")
                contents))))

  (advice-add 'org-html--details :override #'my/org-html--details))

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

(map! :n "C-d" (cmd! (evil-scroll-down nil) (recenter))
      :n "C-u" (cmd! (evil-scroll-up nil) (recenter)))

(map! :n "gj" 'evil-next-visual-line
      :n "gk" 'evil-previous-visual-line)

;; (map! "C-b" #'neotree-toggle)

;; I would also like to add the ability to center when searching

(map! :leader
      :desc "Toggle Olivetti mode" "z" #'olivetti-mode)

;; I don't want line wrap on files that are not markdown or org.
(defun my-disable-line-wrap ()
  "Disable line wrap except for Org and Markdown modes."
  (unless (or (derived-mode-p 'org-mode 'markdown-mode))
    (setq truncate-lines t)))  ;; Disable line wrapping

(add-hook 'find-file-hook #'my-disable-line-wrap)

;; Set the fill column to 80 by default
(setq fill-column 80)

;; Exclude .md and .org files from having the fill column set
;; (add-hook 'markdown-mode-hook (lambda () (setq fill-column nil)))
;; (add-hook 'org-mode-hook (lambda () (setq fill-column nil)))

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

;; Need to come up with a keybind for spellchecking, the current on is ass

(setq company-minimum-prefix-length 1)
(setq scroll-margin 8) ;; or whatever number you want

;; (setq treesit--enabled-p nil)

(defun my/org-roam-random-draft-note ()
  "Open a random Org-roam note tagged with :draft:."
  (interactive)
  (let* ((draft-nodes (org-roam-db-query
                       [:select [id file title]
                                :from tags
                                :inner :join nodes :on (= tags.node-id nodes.id)
                                :where (= tag "draft")]))
         (count (length draft-nodes)))
    (if (> count 0)
        (let* ((selected (nth (random count) draft-nodes))
               (file (nth 1 selected))
               (title (nth 2 selected)))
          (message "Opening draft note: %s" title)
          (find-file file))
      (message "No draft notes found."))))
