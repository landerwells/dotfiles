;;; folgezett.el --- Folgezettel IDs for Org-roam -*- lexical-binding: t; -*-

;; Author: Your Name <landerwells@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org-roam "2.0"))
;; Keywords: zettelkasten, org-roam, notes, folgezettel
;; URL: https://github.com/you/folgezett

;;; Commentary:
;;
;; folgezett.el adds Luhmann-style alphanumeric folgezettel IDs to your
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
;; Section/heading notes use uppercase letters:
;;   1A         - structure note under section 1
;;
;; Usage:
;;   After installing, add the following to your init.el:
;;
;;     (use-package folgezett
;;       :load-path "/path/to/folgezett/"
;;       :after org-roam
;;       :config
;;       (folgezett-setup))
;;
;; This will hook into org-roam-capture so that every new note
;; prompts you to select a parent.

;;; Code:

(require 'org-roam)
(require 'org-roam-node)

;;; ============================================================
;;; Customization
;;; ============================================================

(defgroup folgezett nil
  "Folgezettel support for Org-roam."
  :group 'org-roam
  :prefix "folgezett-")

(defcustom folgezett-property "FOLGEZETT_ID"
  "The org property name used to store the folgezettel ID."
  :type 'string
  :group 'folgezett)

(defcustom folgezett-parent-property "FOLGEZETT_PARENT"
  "The org property name used to store the parent note's org-roam ID."
  :type 'string
  :group 'folgezett)

(defcustom folgezett-top-level-separator "."
  "Separator between the top-level thread number and the first note number.
Bob Doto uses '.' (e.g. 1.1), Luhmann used '/' (e.g. 1/1).
Change this before creating any notes; mixing separators is not supported."
  :type 'string
  :group 'folgezett)

(defcustom folgezett-use-filename nil
  "When non-nil, encode the folgezettel ID into the note's filename.
On ID assignment or reparenting the file is renamed to:
  {FID}{folgezett-filename-separator}{slug}.org
The org-roam DB is re-synced automatically after the rename."
  :type 'boolean
  :group 'folgezett)

(defcustom folgezett-filename-separator "--"
  "String placed between the folgezettel ID and the title slug in the filename.
Only used when `folgezett-use-filename' is non-nil."
  :type 'string
  :group 'folgezett)

;;; ============================================================
;;; ID Parsing and Generation
;;; ============================================================

(defun folgezett--parse-id (id)
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
               (is-sep (char-equal ch (aref folgezett-top-level-separator 0)))
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

(defun folgezett--id-type (segment)
  "Return 'digit or 'alpha depending on what SEGMENT contains."
  (if (string-match-p "^[0-9]+$" segment) 'digit 'alpha))

(defun folgezett--increment-segment (segment)
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

(defun folgezett--next-child-segment (parent-last-segment)
  "Return the first child segment type given the PARENT-LAST-SEGMENT.
Children alternate type: digit parent -> alpha child, alpha parent -> digit child."
  (if (eq (folgezett--id-type parent-last-segment) 'digit) "a" "1"))


(defun folgezett--child-id (parent-id)
  "Return the ID for the first child of PARENT-ID.
E.g. \"1.1\" -> \"1.1a\", \"1.1a\" -> \"1.1a1\"."
  (let* ((segments (folgezett--parse-id parent-id))
         ;; last meaningful segment (skip separator at end)
         (last-seg (car (last (seq-filter
                               (lambda (s) (not (string= s folgezett-top-level-separator)))
                               segments))))
         (child-start (folgezett--next-child-segment last-seg)))
    (concat parent-id child-start)))

(defun folgezett--sibling-id (sibling-id)
  "Return the ID that would follow SIBLING-ID at the same level.
E.g. \"1.1a\" -> \"1.1b\", \"1.1\" -> \"1.2\", \"1.2a3\" -> \"1.2a4\"."
  (let* ((segments (folgezett--parse-id sibling-id))
         (non-sep (seq-filter
                   (lambda (s) (not (string= s folgezett-top-level-separator)))
                   segments))
         (last-seg (car (last non-sep)))
         (incremented (folgezett--increment-segment last-seg))
         ;; Rebuild: everything up to but not including the last segment
         (prefix-segments (butlast segments))
         ;; Drop trailing separator from prefix if present
         (prefix (mapconcat #'identity prefix-segments "")))
    (concat prefix incremented)))

(defun folgezett--new-thread-id ()
  "Return the ID for the first note in a brand-new top-level thread.
Finds the highest existing top-level number and increments it.
If no notes exist, returns \"1.1\"."
  (let* ((existing (folgezett--all-ids))
         (top-level-nums
          (delq nil
                (mapcar (lambda (id)
                          (when (string-match "^\\([0-9]+\\)\\." id)
                            (string-to-number (match-string 1 id))))
                        existing)))
         (next-num (if top-level-nums
                       (1+ (apply #'max top-level-nums))
                     1)))
    (concat (number-to-string next-num) folgezett-top-level-separator "1")))

(defun folgezett--section-id (thread-num)
  "Return the next available section/heading note ID for THREAD-NUM.
Section notes use uppercase letters: 1A, 1B, 1C etc."
  (let* ((existing (folgezett--all-ids))
         (section-ids
          (delq nil
                (mapcar (lambda (id)
                          (when (string-match
                                 (concat "^" (number-to-string thread-num) "\\([A-Z]+\\)$")
                                 id)
                            (match-string 1 id)))
                        existing)))
         (last-section (when section-ids
                         (car (sort section-ids #'string>)))))
    (concat (number-to-string thread-num)
            (if last-section
                (folgezett--increment-segment (downcase last-section))
              "A"))))

;;; ============================================================
;;; Org-roam DB Queries
;;; ============================================================

(defun folgezett--all-ids ()
  "Return a list of all folgezettel IDs currently in the org-roam DB."
  ;; Query node properties for our custom property
  (let ((results
         (org-roam-db-query
          [:select [properties] :from nodes])))
    (delq nil
          (mapcar (lambda (row)
                    (let* ((props (car row))
                           (fid (cdr (assoc folgezett-property props))))
                      fid))
                  results))))

(defun folgezett--nodes-with-ids ()
  "Return an alist of (folgezett-ID . ORG-ROAM-NODE) for all nodes that have a folgezettel ID."
  (let ((all-nodes (org-roam-node-list))
        result)
    (dolist (node all-nodes result)
      (let* ((props (org-roam-node-properties node))
             (fid (cdr (assoc folgezett-property props))))
        (when fid
          (push (cons fid node) result))))))

(defun folgezett--node-by-fid (fid)
  "Return the org-roam node with folgezettel ID FID, or nil."
  (cdr (assoc fid (folgezett--nodes-with-ids))))


(defun folgezett--children-of (fid)
  "Return a sorted list of folgezettel IDs that are direct children of FID."
  (let* ((all-ids (folgezett--all-ids))
         (child-pattern (regexp-quote fid))
         ;; A direct child of "1.1" matches "1.1a", "1.1b" but NOT "1.1a1"
         ;; It appends exactly one alpha or digit segment with no further nesting
         (direct-child-re (concat "^" child-pattern "[a-z][^a-z0-9]*$\\|^" child-pattern "[0-9]+$")))
    (sort (seq-filter (lambda (id)
                        ;; Must start with parent id and add exactly one segment
                        (and (string-prefix-p fid id)
                             (not (string= fid id))
                             (let ((suffix (substring id (length fid))))
                               (string-match-p "^[a-z0-9]+$" suffix)
                               ;; suffix must be all alpha OR all digit (one segment)
                               (or (string-match-p "^[a-z]+$" suffix)
                                   (string-match-p "^[0-9]+$" suffix)))))
                      all-ids)
          #'string<)))

(defun folgezett--next-sibling-of (fid)
  "Find the next available sibling ID after FID, skipping any that are taken."
  (let ((candidate (folgezett--sibling-id fid))
        (all-ids (folgezett--all-ids)))
    (while (member candidate all-ids)
      (setq candidate (folgezett--sibling-id candidate)))
    candidate))

(defun folgezett--next-child-of (fid)
  "Find the next available child ID under FID, skipping any that are taken."
  (let ((candidate (folgezett--child-id fid))
        (all-ids (folgezett--all-ids)))
    (while (member candidate all-ids)
      (setq candidate (folgezett--next-sibling-of candidate)))
    candidate))

;;; ============================================================
;;; Interactive Selection
;;; ============================================================

(defun folgezett--select-parent-node ()
  "Prompt the user to select a parent node from all nodes that have a folgezettel ID.
Returns the selected org-roam node, or nil if the user chooses 'New thread'."
  (let* ((nodes-with-ids (folgezett--nodes-with-ids))
         ;; Build candidates: "FID  Title" -> node
         (candidates
          (mapcar (lambda (pair)
                    (let* ((fid (car pair))
                           (node (cdr pair))
                           (title (org-roam-node-title node))
                           (label (format "%-15s %s" fid title)))
                      (cons label node)))
                  (sort nodes-with-ids
                        (lambda (a b) (string< (car a) (car b))))))
         (new-thread-option "[ New top-level thread ]")
         (all-candidates (cons (cons new-thread-option nil) candidates))
         (choice (completing-read
                  "Place note behind (parent): "
                  (mapcar #'car all-candidates)
                  nil t)))
    (cdr (assoc choice all-candidates))))

(defun folgezett--prompt-note-type ()
  "Prompt for whether this is a regular note or a section/heading note."
  (let ((choice (completing-read
                 "Note type: "
                 '("regular" "section/heading")
                 nil t nil nil "regular")))
    (intern choice)))

;;; ============================================================
;;; Filename Encoding
;;; ============================================================

(defun folgezett--buffer-title ()
  "Return the #+title of the current org buffer, or nil if absent."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^#\\+title:\\s-*\\(.+\\)$" nil t)
      (match-string-no-properties 1))))

(defun folgezett--title-to-slug (title)
  "Return a filesystem-safe slug derived from TITLE.
Downcases, replaces non-alphanumeric runs with hyphens, and trims edge hyphens."
  (string-trim
   (replace-regexp-in-string "-+" "-"
     (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase title)))
   "-" "-"))

(defun folgezett--rename-buffer-file (fid)
  "Rename the current buffer's file to encode FID in the filename.
The new name is `{FID}{folgezett-filename-separator}{slug}.org'.
Does nothing when `folgezett-use-filename' is nil or the buffer has no file."
  (when (and folgezett-use-filename (buffer-file-name))
    (let* ((current-file (buffer-file-name))
           (dir (file-name-directory current-file))
           (title (or (folgezett--buffer-title) "note"))
           (slug (folgezett--title-to-slug title))
           (new-name (concat fid folgezett-filename-separator slug ".org"))
           (new-path (expand-file-name new-name dir)))
      (unless (string= (expand-file-name current-file) new-path)
        (rename-file current-file new-path t)
        (set-visited-file-name new-path t t)
        (org-roam-db-sync)
        (message "folgezett: renamed to %s" new-name)))))

;;; ============================================================
;;; Writing Properties to the Current Buffer
;;; ============================================================

(defun folgezett--write-properties (fid parent-node)
  "Write the folgezettel ID FID and parent link into the current org buffer's PROPERTIES drawer.
PARENT-NODE is the org-roam node of the parent, or nil for a top-level note."
  (org-roam-add-property fid folgezett-property)
  (when parent-node
    (org-roam-add-property
     (org-roam-node-id parent-node)
     folgezett-parent-property)))

;;; ============================================================
;;; Main Entry Point (called from capture hook)
;;; ============================================================

;;;###autoload
(defun folgezett-assign-id ()
  "Interactively assign a folgezettel ID to the current org-roam note being captured.
This should be called from `org-roam-capture-new-node-hook'."
  (interactive)
  (let* ((note-type (folgezett--prompt-note-type))
         (parent-node (folgezett--select-parent-node))
         (fid
          (cond
           ;; Section note under an existing thread
           ((and (eq note-type 'section/heading) parent-node)
            (let* ((parent-props (org-roam-node-properties parent-node))
                   (parent-fid (cdr (assoc folgezett-property parent-props)))
                   (thread-num (when (string-match "^\\([0-9]+\\)" parent-fid)
                                 (string-to-number (match-string 1 parent-fid)))))
              (folgezett--section-id thread-num)))
           ;; Section note at top level (no parent selected)
           ((and (eq note-type 'section/heading) (null parent-node))
            (let* ((all-ids (folgezett--all-ids))
                   (top-nums (delq nil
                                   (mapcar (lambda (id)
                                             (when (string-match "^\\([0-9]+\\)\\." id)
                                               (string-to-number (match-string 1 id))))
                                           all-ids)))
                   (thread-num (if top-nums (apply #'max top-nums) 1)))
              (folgezett--section-id thread-num)))
           ;; New top-level thread
           ((null parent-node)
            (folgezett--new-thread-id))
           ;; Regular child note
           (t
            (let* ((parent-props (org-roam-node-properties parent-node))
                   (parent-fid (cdr (assoc folgezett-property parent-props))))
              (folgezett--next-child-of parent-fid))))))
    (folgezett--write-properties fid parent-node)
    (folgezett--rename-buffer-file fid)
    (message "Folgezettel ID assigned: %s" fid)))

;;; ============================================================
;;; Reparenting and Tree Rebalancing
;;; ============================================================

;;;###autoload
(defun folgezett-reparent-note ()
  "Interactively reparent the current note to a new parent, reassigning its folgezettel ID.
This does NOT recursively renumber descendants — that is intentional per Luhmann's method:
IDs are fixed once assigned and relationships are conveyed through linking."
  (interactive)
  (unless (org-roam-buffer-p)
    (user-error "Not in an org-roam buffer"))
  (let* ((current-node (org-roam-node-at-point 'assert))
         (current-props (org-roam-node-properties current-node))
         (old-fid (cdr (assoc folgezett-property current-props))))
    (unless old-fid
      (user-error "Current note has no folgezettel ID"))
    (let* ((new-parent (folgezett--select-parent-node))
           (new-fid
            (if (null new-parent)
                (folgezett--new-thread-id)
              (let* ((parent-props (org-roam-node-properties new-parent))
                     (parent-fid (cdr (assoc folgezett-property parent-props))))
                (folgezett--next-child-of parent-fid)))))
      (when (yes-or-no-p (format "Reparent note from %s to %s? " old-fid new-fid))
        (org-roam-remove-property folgezett-property)
        (org-roam-remove-property folgezett-parent-property)
        (folgezett--write-properties new-fid new-parent)
        (folgezett--rename-buffer-file new-fid)
        (message "Note reparented: %s -> %s" old-fid new-fid)))))

;;; ============================================================
;;; Org-roam Node Display: Custom Column
;;; ============================================================

(defun folgezett-node-folgezettel-id (node)
  "Return the folgezettel ID for NODE, or an empty string if none."
  (let* ((props (org-roam-node-properties node))
         (fid (cdr (assoc folgezett-property props))))
    (or fid "")))

;;;###autoload
(defun folgezett-setup-display ()
  "Configure `org-roam-node-display-template' to show folgezettel IDs.
Call this after org-roam is loaded. The folgezettel ID will appear
as a left-aligned column before the note title."
  (setq org-roam-node-display-template
        (concat "${folgezett-id:12} ${title:*} ${tags:20}")))

;; Register the slot so org-roam's formatter can find it
(cl-defmethod org-roam-node-folgezett-id ((node org-roam-node))
  "Return the folgezettel ID for NODE for use in display templates."
  (folgezett-node-folgezettel-id node))

;;; ============================================================
;;; Hook Setup
;;; ============================================================

;;;###autoload
(defun folgezett-setup ()
  "Set up folgezett hooks and display configuration.
Call this in your init.el after org-roam is loaded:

  (with-eval-after-load 'org-roam
    (folgezett-setup))"
  (add-hook 'org-roam-capture-new-node-hook #'folgezett-assign-id)
  (folgezett-setup-display)
  (message "folgezett: hooks and display configured."))

;;; ============================================================
;;; Keybindings (optional prefix map)
;;; ============================================================

(defvar folgezett-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'folgezett-reparent-note)
    (define-key map (kbd "i") #'folgezett-assign-id)
    map)
  "Keymap for folgezett commands. Bind this to a prefix of your choice.
Example: (global-set-key (kbd \"C-c z\") folgezett-map)")

(provide 'folgezett)
;;; folgezett.el ends here
