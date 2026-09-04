;;; org-roam-export.el --- Org-roam org-export tweaks -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2025 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This package provides the necessary changes required to make org-export work out-of-the-box.
;;
;; To enable it, run:
;;
;;    (require 'org-roam-export)
;;
;; The key issue Org's export-to-html functionality has is that it does not respect the ID property, which
;; Org-roam relies heavily on. This patches the necessary function in ox-html to export ID links correctly,
;; pointing to the correct place.
;;
;;; Code:
(provide 'org-roam-export)
;;; org-roam-export.el ends here
