;;; clang-formatter.el --- clang-format interface for emacs.
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2023 qxxt (GNU/GPL Licence)

;; Authors: qxxt
;; URL: https://github.com/qxxt/clang-formatter.el
;; Package-Requires: ((emacs) (project))
;; Keywords: clang-format

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Small function for formatting current buffer using ‘clang-format’.

;;; Code:

(require 'project)

(defgroup clang-format nil
  "Group for clang-format."
  :group 'languages)

(defcustom clang-format-command "clang-format"
  "The ‘clang-format’ command."
  :type 'string
  :group 'clang-format
  :risky t)

(defcustom clang-format-style "Google"
  "The default arguments for clang-format."
  :type 'string
  :group 'clang-format)

(defcustom clang-format-c-style nil
  "Clang-format arguments for ‘c-mode’.
Override the default argument."
  :type 'string
  :group 'clang-format)

(defcustom clang-format-c++-style nil
  "Clang-format arguments for ‘c++-mode’.
Override the default argument."
  :type 'string
  :group 'clang-format)

(defcustom clang-format-java-style nil
  "Clang-format arguments for ‘java-mode’.
Override the default argument."
  :type 'string
  :group 'clang-format)

(defcustom clang-format-javascript-style nil
  "Clang-format arguments for ‘javascript-mode’.
Override the default argument."
  :type 'string
  :group 'clang-format)

(defcustom clang-format-objc-style nil
  "Clang-format arguments for ‘objc-mode’.
Override the default argument."
  :type 'string
  :group 'clang-format)

(defcustom clang-format-csharp-style nil
  "Clang-format arguments for ‘csharp-mode’.
Override the default argument."
  :type 'string
  :group 'clang-format)

(defcustom clang-format-protobuf-style nil
  "Clang-format arguments for ‘protobuf-mode’.
Override the default argument."
  :type 'string
  :group 'clang-format)

(defun clang-format-buffer ()
  "Format current buffer with clang-format."
  (interactive)
  (if (not (executable-find clang-format-command))
      (error "Command not found: ‘%s’" clang-format-command))

  (let ((tmp-file (unless (null buffer-file-name)
                    (file-name-nondirectory buffer-file-name)))
        (command clang-format-command)
        (style (concat "--style=" clang-format-style))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))

    ;; Replace ‘style’ with mode specific if provided.
    ;; Adds filename if users use non-file buffer which has
    ;; no filename.
    (cond
      ((eq major-mode 'c-mode)
       (if (not (null clang-format-c-style))
           (setf style clang-format-c-style))
       (if (null tmp-file)
           (setf tmp-file "tmp.c")))
      ((eq major-mode 'c++-mode)
       (if (not (null clang-format-c++-style))
           (setf style clang-format-c++-style))
       (if (null tmp-file)
           (setf tmp-file "tmp.cc")))
      ((eq major-mode 'java-mode)
       (if (not (null clang-format-java-style))
           (setf style clang-format-java-style))
       (if (null tmp-file)
           (setf tmp-file "tmp.java")))
      ((eq major-mode 'javascript-mode)
       (if (not (null clang-format-javascript-style))
           (setf style clang-format-javascript-style))
       (if (null tmp-file)
           (setf tmp-file "tmp.js")))
      ((eq major-mode 'objc-mode)
       (if (not (null clang-format-objc-style))
           (setf style clang-format-objc-style))
       (if (null tmp-file)
           (setf tmp-file "tmp.mm")))
      ((eq major-mode 'csharp-mode)
       (if (not (null clang-format-csharp-style))
           (setf style clang-format-csharp-style))
       (if (null tmp-file)
           (setf tmp-file "tmp.cs")))
      ((eq major-mode 'protobuf-mode)
       (if (not (null clang-format-protobuf-style))
           (setf style clang-format-protobuf-style))
       (if (null tmp-file)
           (setf tmp-file "tmp.proto")))
      (t (error (concat "Mode not supported: " (symbol-name major-mode)))))

    ;; Expands %R to root vc directory.
    ;; Useful for specifying .clang-format file.
    ;; eg. --style="%R/.clang-format"
    (if (string-search "%R" style)
        (if (null (project-root (project-current)))
            (error "The arguments contains macro for root directory but unable find vc root directory")
          (setf style (string-replace "%R" (project-root (project-current)) style))))

    ;; Create ‘temp-file’ and write the content of ‘current-buffer’ to
    ;; it.
    (with-current-buffer (current-buffer)
      (setf tmp-file (make-temp-file "format-" nil (concat "-" tmp-file) (buffer-string))))

    (let ((tmp-buf (get-buffer-create (concat "*clang-format: tmp*"))))
      (with-current-buffer tmp-buf
        (setq buffer-read-only nil)
        (erase-buffer))

      ;; Dry run formatter to check if there is a need for
      ;; formatting. And write the output to ‘tmp-buf’.
      ;;
      ;; "--dry-run" flag will give empty output if the
      ;; input is already formatted. This eliminates
      ;; the need for diff checking.
      (if (not (zerop (call-process command nil tmp-buf nil style "--dry-run" "--ferror-limit=1" "--fno-color-diagnostics" tmp-file)))
          (message "Error while calling ’%s’" (concat command " " style " --dry-run --ferror-limit=1 --fno-color-diagnostics " tmp-file))

        ;; Check if the ‘tmp-buf’ is empty.
        (if (zerop (buffer-size tmp-buf))
            (message "Buffer already formatted")

          (let ((msg-buf (get-buffer-create "*clang-format: msg*"))
                (patch-buf (get-buffer-create (concat "*clang-format: " tmp-file "*"))))

            ;; Replace the ‘msg-buf’ content with ‘tmp-buf’.
            (with-current-buffer msg-buf
              (setq buffer-read-only nil)
              (replace-buffer-contents tmp-buf)
              (setq buffer-read-only t))

            ;; Prepares a ‘patch-buff’.
            (with-current-buffer patch-buf
              (setq buffer-read-only nil)
              (erase-buffer))

            ;; Call formatter and output to ‘patch-buf’.
            (if (not (zerop (call-process command nil patch-buf nil style tmp-file)))
                (message "Error while calling ’%s’" (concat command " " style " " tmp-file))
              ;; Replace the content of current buffer with
              ;; ‘patch-buf’.
              (message "Formatted with ‘%s’" (concat command " " style " " tmp-file))
              (replace-buffer-contents patch-buf))
            (kill-buffer patch-buf))))
      (kill-buffer tmp-buf))
    (delete-file tmp-file)))

(defun clang-format-before-save ()
  "Formats all supported modes before save."
  (dolist (hook '(c-mode-hook c++-mode-hook java-mode-hook javascript-mode-hook objc-mode-hook csharp-mode-hook protobuf-mode-hook))
    (add-hook hook (lambda () (add-hook 'before-save-hook 'clang-format-buffer nil 'local)))))

(provide 'clang-formatter)
;;; clang-formatter.el ends here
