;;; folgezettel.el --- Folgezettel IDs for Org-roam -*- lexical-binding: t; -*-

;; Author: Your Name <landerwells@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (org-roam "2.0"))
;; Keywords: zettelkasten, org-roam, notes, folgezettel
;; URL: https://github.com/you/folgezettel

;;; Commentary:
;;
;; folgezettel.el adds Luhmann-style alphanumeric folgezettel IDs to your
;; Org-roam zettelkasten.
;;
;; ID format follows the pattern described by Bob Doto:
;;   Numbers and letters alternate at each branching level.
;;   Siblings share a numeric prefix and increment their last segment.
;;   Children branch by appending the next type of character.
;;
;; Examples:
;;   1.1        - first note in thread 1
;;   1.1a       - child of 1.1
;;   1.1a1      - child of 1.1a
;;   1.1a1b     - child of 1.1a1
;;   1.2        - sibling of 1.1 (shares parent thread "1")
;;   1.1b       - sibling of 1.1a
;;
;; Usage:
;;   After installing, add the following to your init.el:
;;
;;     (use-package folgezettel
;;       :load-path "/path/to/folgezettel/"
;;       :after org-roam
;;       :config
;;       (folgezettel-setup))
;;
;; By default, only notes created with the "m" (main) capture template
;; will be assigned a folgezettel ID. Customize `folgezettel-capture-template-keys'
;; to change this.
;;
;; When capturing, you will be asked whether to place the note under an existing
;; parent. The default is no parent (a new top-level thread). Press RET to accept
;; the default, or type to search for a parent note.

;;; Code:

(require 'org-roam)
(require 'org-roam-node)

;;; ============================================================
;;; Customization
;;; ============================================================

(defgroup folgezettel nil
  "Folgezettel support for Org-roam."
  :group 'org-roam
  :prefix "folgezettel-")

(defcustom folgezettel-property "FOLGEZETTEL_ID"
  "The org property name used to store the folgezettel ID."
  :type 'string
  :group 'folgezettel)

(defcustom folgezettel-parent-property "FOLGEZETTEL_PARENT"
  "The org property name used to store the parent note's org-roam ID."
  :type 'string
  :group 'folgezettel)

(defcustom folgezettel-top-level-separator "."
  "Separator between the top-level thread number and the first note number.
Bob Doto uses '.' (e.g. 1.1), Luhmann used '/' (e.g. 1/1).
Change this before creating any notes; mixing separators is not supported."
  :type 'string
  :group 'folgezettel)

(defcustom folgezettel-use-filename nil
  "When non-nil, encode the folgezettel ID into the note's filename.
On ID assignment or reparenting the file is renamed to:
  {FID}{folgezettel-filename-separator}{slug}.org
The org-roam DB is re-synced automatically after the rename."
  :type 'boolean
  :group 'folgezettel)

(defcustom folgezettel-filename-separator "--"
  "String placed between the folgezettel ID and the title slug in the filename.
Only used when `folgezettel-use-filename' is non-nil."
  :type 'string
  :group 'folgezettel)

(defcustom folgezettel-capture-template-keys '("m")
  "List of org-roam capture template keys that should receive folgezettel IDs.
Only captures using one of these keys will trigger ID assignment.
The default is '(\"m\") for the main notebox template."
  :type '(repeat string)
  :group 'folgezettel)

;;; ============================================================
;;; ID Parsing and Generation
;;; ============================================================

(defun folgezettel--parse-id (id)
  "Parse a folgezettel ID string into a list of segments.
Each segment is a string of either all-digits or all-letters.
Example: \"1.2a3b\" -> (\"1\" \".\" \"2\" \"a\" \"3\" \"b\")"
  (when (and id (not (string-empty-p id)))
    (let ((segments '())
          (i 0)
          (len (length id)))
      (while (< i len)
        (let* ((ch (aref id i))
               (is-digit (and (>= ch ?0) (<= ch ?9)))
               (is-alpha (or (and (>= ch ?a) (<= ch ?z))
                             (and (>= ch ?A) (<= ch ?Z))))
               (is-sep (char-equal ch (aref folgezettel-top-level-separator 0)))
               (j (1+ i)))
          (cond
           (is-sep
            (push (string ch) segments)
            (setq i j))
           ((or is-digit is-alpha)
            ;; collect run of same character class
            (while (and (< j len)
                        (let ((c (aref id j)))
                          (if is-digit
                              (and (>= c ?0) (<= c ?9))
                            (or (and (>= c ?a) (<= c ?z))
                                (and (>= c ?A) (<= c ?Z))))))
              (setq j (1+ j)))
            (push (substring id i j) segments)
            (setq i j))
           (t (setq i (1+ i))))))
      (nreverse segments))))

(defun folgezettel--id-type (segment)
  "Return 'digit or 'alpha depending on what SEGMENT contains."
  (if (string-match-p "^[0-9]+$" segment) 'digit 'alpha))

(defun folgezettel--increment-segment (segment)
  "Return the next value after SEGMENT.
Digits increment numerically: \"1\" -> \"2\", \"9\" -> \"10\".
Letters increment alphabetically: \"a\" -> \"b\", \"z\" -> \"aa\"."
  (if (string-match-p "^[0-9]+$" segment)
      (number-to-string (1+ (string-to-number segment)))
    ;; alphabetic: treat like base-26
    (let* ((chars (string-to-list segment))
           (carry t)
           (result '()))
      (dolist (ch (nreverse chars))
        (if carry
            (if (char-equal ch ?z)
                (progn (push ?a result) (setq carry t))
              (push (1+ ch) result)
              (setq carry nil))
          (push ch result)))
      (when carry (push ?a result))
      (apply #'string result))))

(defun folgezettel--next-child-segment (parent-last-segment)
  "Return the first child segment type given the PARENT-LAST-SEGMENT.
Children alternate type: digit parent -> alpha child, alpha parent -> digit child."
  (if (eq (folgezettel--id-type parent-last-segment) 'digit) "a" "1"))


(defun folgezettel--child-id (parent-id)
  "Return the ID for the first child of PARENT-ID.
E.g. \"1.1\" -> \"1.1a\", \"1.1a\" -> \"1.1a1\"."
  (let* ((segments (folgezettel--parse-id parent-id))
         ;; last meaningful segment (skip separator at end)
         (last-seg (car (last (seq-filter
                               (lambda (s) (not (string= s folgezettel-top-level-separator)))
                               segments))))
         (child-start (folgezettel--next-child-segment last-seg)))
    (concat parent-id child-start)))

(defun folgezettel--sibling-id (sibling-id)
  "Return the ID that would follow SIBLING-ID at the same level.
E.g. \"1.1a\" -> \"1.1b\", \"1.1\" -> \"1.2\", \"1.2a3\" -> \"1.2a4\"."
  (let* ((segments (folgezettel--parse-id sibling-id))
         (non-sep (seq-filter
                   (lambda (s) (not (string= s folgezettel-top-level-separator)))
                   segments))
         (last-seg (car (last non-sep)))
         (incremented (folgezettel--increment-segment last-seg))
         ;; Rebuild: everything up to but not including the last segment
         (prefix-segments (butlast segments))
         ;; Drop trailing separator from prefix if present
         (prefix (mapconcat #'identity prefix-segments "")))
    (concat prefix incremented)))

(defun folgezettel--new-thread-id ()
  "Return the ID for the first note in a brand-new top-level thread.
Finds the highest existing top-level number and increments it.
If no notes exist, returns \"1.1\"."
  (let* ((existing (folgezettel--all-ids))
         (top-level-nums
          (delq nil
                (mapcar (lambda (id)
                          (when (string-match "^\\([0-9]+\\)\\." id)
                            (string-to-number (match-string 1 id))))
                        existing)))
         (next-num (if top-level-nums
                       (1+ (apply #'max top-level-nums))
                     1)))
    (concat (number-to-string next-num) folgezettel-top-level-separator "1")))

;;; ============================================================
;;; Org-roam DB Queries
;;; ============================================================

(defun folgezettel--all-ids ()
  "Return a list of all folgezettel IDs currently in the org-roam DB."
  (let ((results
         (org-roam-db-query
          [:select [properties] :from nodes])))
    (delq nil
          (mapcar (lambda (row)
                    (let* ((props (car row))
                           (fid (cdr (assoc folgezettel-property props))))
                      fid))
                  results))))

(defun folgezettel--nodes-with-ids ()
  "Return an alist of (folgezettel-ID . ORG-ROAM-NODE) for all nodes that have a folgezettel ID."
  (let ((all-nodes (org-roam-node-list))
        result)
    (dolist (node all-nodes result)
      (let* ((props (org-roam-node-properties node))
             (fid (cdr (assoc folgezettel-property props))))
        (when fid
          (push (cons fid node) result))))))

(defun folgezettel--node-by-fid (fid)
  "Return the org-roam node with folgezettel ID FID, or nil."
  (cdr (assoc fid (folgezettel--nodes-with-ids))))

(defun folgezettel--children-of (fid)
  "Return a sorted list of folgezettel IDs that are direct children of FID."
  (sort (seq-filter (lambda (id)
                      (and (string-prefix-p fid id)
                           (not (string= fid id))
                           (let ((suffix (substring id (length fid))))
                             (or (string-match-p "^[a-z]+$" suffix)
                                 (string-match-p "^[0-9]+$" suffix)))))
                    (folgezettel--all-ids))
        #'string<))

(defun folgezettel--next-sibling-of (fid)
  "Find the next available sibling ID after FID, skipping any that are taken."
  (let ((candidate (folgezettel--sibling-id fid))
        (all-ids (folgezettel--all-ids)))
    (while (member candidate all-ids)
      (setq candidate (folgezettel--sibling-id candidate)))
    candidate))

(defun folgezettel--next-child-of (fid)
  "Find the next available child ID under FID, skipping any that are taken."
  (let ((candidate (folgezettel--child-id fid)))
    (if (member candidate (folgezettel--all-ids))
        (folgezettel--next-sibling-of candidate)
      candidate)))

;;; ============================================================
;;; Interactive Selection
;;; ============================================================

(defun folgezettel--select-parent-node ()
  "Prompt the user to optionally select a parent node.
The default is no parent (new top-level thread). Returns the selected
org-roam node, or nil if the user keeps the default."
  (let* ((nodes-with-ids (folgezettel--nodes-with-ids))
         (candidates
          (mapcar (lambda (pair)
                    (let* ((fid (car pair))
                           (node (cdr pair))
                           (title (org-roam-node-title node))
                           (label (format "%-15s %s" fid title)))
                      (cons label node)))
                  (sort nodes-with-ids
                        (lambda (a b) (string< (car a) (car b))))))
         (no-parent-option "[ No parent — new top-level thread ]")
         (all-candidates (cons (cons no-parent-option nil) candidates))
         (choice (completing-read
                  "Parent note (default: none): "
                  (mapcar #'car all-candidates)
                  nil t nil nil no-parent-option)))
    (cdr (assoc choice all-candidates))))

;;; ============================================================
;;; Capture Template Key Check
;;; ============================================================

(defun folgezettel--active-capture-key ()
  "Return the key of the currently active org-roam capture template, or nil."
  (when (bound-and-true-p org-roam-capture--template)
    (car org-roam-capture--template)))

(defun folgezettel--should-assign-p ()
  "Return non-nil if the current capture should receive a folgezettel ID.
Checks `folgezettel-capture-template-keys' against the active template key."
  (let ((key (folgezettel--active-capture-key)))
    (member key folgezettel-capture-template-keys)))

;;; ============================================================
;;; Filename Encoding
;;; ============================================================

(defun folgezettel--buffer-title ()
  "Return the #+title of the current org buffer, or nil if absent."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+title:\\s-*\\(.+\\)$" nil t)
      (match-string-no-properties 1))))

(defun folgezettel--title-to-slug (title)
  "Return a filesystem-safe slug derived from TITLE."
  (string-trim
   (replace-regexp-in-string "-+" "-"
                             (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase title)))
   "-" "-"))

(defun folgezettel--rename-buffer-file (fid)
  "Rename the current buffer's file to encode FID in the filename.
Does nothing when `folgezettel-use-filename' is nil or the buffer has no file."
  (when (and folgezettel-use-filename (buffer-file-name))
    (let* ((current-file (buffer-file-name))
           (dir (file-name-directory current-file))
           (title (or (folgezettel--buffer-title) "note"))
           (slug (folgezettel--title-to-slug title))
           (new-name (concat fid folgezettel-filename-separator slug ".org"))
           (new-path (expand-file-name new-name dir)))
      (unless (string= (expand-file-name current-file) new-path)
        (rename-file current-file new-path t)
        (set-visited-file-name new-path t t)
        (org-roam-db-sync)
        (message "folgezettel: renamed to %s" new-name)))))

;;; ============================================================
;;; Writing Properties to the Current Buffer
;;; ============================================================

(defun folgezettel--write-properties (fid parent-node)
  "Write the folgezettel ID FID and parent link into the current org buffer.
PARENT-NODE is the org-roam node of the parent, or nil for a top-level note."
  (org-roam-add-property fid folgezettel-property)
  (when parent-node
    (org-roam-add-property
     (org-roam-node-id parent-node)
     folgezettel-parent-property)))

;;; ============================================================
;;; Main Entry Point (called from capture hook)
;;; ============================================================

;;;###autoload
(defun folgezettel-assign-id ()
  "Interactively assign a folgezettel ID to the current org-roam note being captured.
This should be called from `org-roam-capture-new-node-hook'.
Only runs for templates listed in `folgezettel-capture-template-keys'."
  (interactive)
  (when (or (called-interactively-p 'any) (folgezettel--should-assign-p))
    (let* ((parent-node (folgezettel--select-parent-node))
           (fid
            (if (null parent-node)
                (folgezettel--new-thread-id)
              (let* ((parent-props (org-roam-node-properties parent-node))
                     (parent-fid (cdr (assoc folgezettel-property parent-props))))
                (folgezettel--next-child-of parent-fid)))))
      (folgezettel--write-properties fid parent-node)
      (folgezettel--rename-buffer-file fid)
      (message "Folgezettel ID assigned: %s" fid))))

;;; ============================================================
;;; Reparenting
;;; ============================================================

;;;###autoload
(defun folgezettel-reparent-note ()
  "Interactively reparent the current note to a new parent, reassigning its folgezettel ID.
This does NOT recursively renumber descendants — IDs are fixed once assigned."
  (interactive)
  (unless (org-roam-buffer-p)
    (user-error "Not in an org-roam buffer"))
  (let* ((current-node (org-roam-node-at-point 'assert))
         (current-props (org-roam-node-properties current-node))
         (old-fid (cdr (assoc folgezettel-property current-props))))
    (unless old-fid
      (user-error "Current note has no folgezettel ID"))
    (let* ((new-parent (folgezettel--select-parent-node))
           (new-fid
            (if (null new-parent)
                (folgezettel--new-thread-id)
              (let* ((parent-props (org-roam-node-properties new-parent))
                     (parent-fid (cdr (assoc folgezettel-property parent-props))))
                (folgezettel--next-child-of parent-fid)))))
      (when (yes-or-no-p (format "Reparent note from %s to %s? " old-fid new-fid))
        (org-roam-remove-property folgezettel-property)
        (org-roam-remove-property folgezettel-parent-property)
        (folgezettel--write-properties new-fid new-parent)
        (folgezettel--rename-buffer-file new-fid)
        (message "Note reparented: %s -> %s" old-fid new-fid)))))

;;; ============================================================
;;; Org-roam Node Display: Custom Column
;;; ============================================================

(defun folgezettel-node-folgezettel-id (node)
  "Return the folgezettel ID for NODE, or an empty string if none."
  (let* ((props (org-roam-node-properties node))
         (fid (cdr (assoc folgezettel-property props))))
    (or fid "")))

;;;###autoload
(defun folgezettel-setup-display ()
  "Configure `org-roam-node-display-template' to show folgezettel IDs."
  (setq org-roam-node-display-template
        (concat "${folgezettel-id:12} ${title:*} ${tags:20}")))

(cl-defmethod org-roam-node-folgezettel-id ((node org-roam-node))
  "Return the folgezettel ID for NODE for use in display templates."
  (folgezettel-node-folgezettel-id node))

;;; ============================================================
;;; Hook Setup
;;; ============================================================

;;;###autoload
(defun folgezettel-setup ()
  "Set up folgezettel hooks and display configuration.
Call this in your init.el after org-roam is loaded:

  (with-eval-after-load 'org-roam
    (folgezettel-setup))"
  (add-hook 'org-roam-capture-new-node-hook #'folgezettel-assign-id)
  (folgezettel-setup-display)
  (message "folgezettel: hooks and display configured."))

;;; ============================================================
;;; Keybindings (optional prefix map)
;;; ============================================================

(defvar folgezettel-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'folgezettel-reparent-note)
    (define-key map (kbd "i") #'folgezettel-assign-id)
    map)
  "Keymap for folgezettel commands. Bind this to a prefix of your choice.
Example: (global-set-key (kbd \"C-c z\") folgezettel-map)")

(provide 'folgezettel)
;;; folgezettel.el ends here
