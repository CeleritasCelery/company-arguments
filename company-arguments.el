;;; company-arguments.el --- company backend for command line arguments  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Troy Hinckley

;; Author: Troy Hinckley <troy.hinckley@gmail.com>
;; URL: https://github.com/CeleritasCelery/company-arguments
;; Version: 0.1.0
;; Package-Requires: ((company "0.9.3") (cl-lib "0.5.0") (ample-regexps "0.1") (dash "2.12.0") (emacs "25"))

;; This file is not part of GNU Emacs.

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

;; provide completion for command line arguments with company. uses
;; the --help command to get all arguments and then parses that list.
;; The annotation shows alternate arguments that are equivilent and
;; the meta data provides the description.

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'ample-regexps)

(defvar company-arguments--proc nil
  "current running process")

(defun company-arguments--get-command ()
  "get the command at the start of the line"
  (save-excursion
    (back-to-indentation)
    (substitute-env-vars
     (buffer-substring (point)
                       (1- (re-search-forward (rx (or space eol))))))))

(defun company-arguments--prefix ()
  "Get the arguments for the command at point"
  (when (and (looking-back (rx space (repeat 1 2 "-") (optional (1+ (any word "_-"))))
                           (line-beginning-position))
             (looking-at-p (rx symbol-end))
             (equal 0 (call-process "which" nil nil nil (company-arguments--get-command))))
    (cons (buffer-substring-no-properties
           (save-excursion
             (1+ (re-search-backward (rx space))))
           (point))
          t)))

(defun company-arguments--candidates (callback prefix)
  "use --help to get arguments that match PREFIX.
When --help is parsed, call CALLBACK."
  (let ((command (company-arguments--get-command))
        (buffer (get-buffer-create "*company-arguments-command-output*")))
    (when (process-live-p company-arguments--proc)
      (kill-process company-arguments--proc))
    (with-current-buffer buffer
      (erase-buffer))
    (setq company-arguments--proc
          (start-process "company-argments-candidates"
                         buffer
                         (locate-file command (split-string (getenv "PATH") ":"))
                         "--help"))
    (set-process-sentinel company-arguments--proc
                          (lambda (_ event)
                            (when (equal event "finished\n")
                              (funcall callback
                                       (seq-sort-by
                                        'length '<
                                        (cl-loop for (arg desc alt) in (company-arguments--parse-help buffer)
                                                 when (string-prefix-p prefix arg)
                                                 collect (progn (put-text-property 0 1 'description desc arg)
                                                                (put-text-property 0 1 'alternate alt arg)
                                                                arg)))))))))

(define-arx arg-rx
  '((spc (1+ space))
    (short (group-n 1 "-" word))
    (long (group-n 2 "--"
                   (1+ (any word "_-"))
                   (optional "[")
                   (optional "=" (1+ word))
                   (optional "]")))))

(defun company-arguments--parse-help (buffer)
  "parse help BUFFER.
command line argument data should be in one the
following forms
  -f, --foo    description
  -f           description
      --foo    description
This function will return an alist of the form
 (argument description alternate-form)"
  (with-current-buffer buffer
    (goto-char (point-min))
    (let (arguments)
      (while (re-search-forward (arg-rx bol spc
                                        (or (seq short ", " long)
                                            long
                                            short)
                                        spc
                                        (group-n 3 (1+ nonl)))
                                nil 'noerror)
        (let ((short (match-string 1))
              (long  (match-string 2))
              (desc  (match-string 3))
              ;; get the rest of the description which might be on
              ;; multiple lines
              (rest (cl-loop initially (forward-line)
                             while (looking-at (rx bol (>= 7 space)
                                                   (group-n 1 (1+ nonl))))
                             collect (match-string 1)
                             do (forward-line))))
          (setq desc (mapconcat 'identity (cons desc rest) " "))
          ;; when two arguments are similar and right after one
          ;; another, the help message will use the form "likewise,
          ;; but <additional info>". Since the description for the
          ;; arguments are displayed one at a time we replace
          ;; "likewise" with the referenced description
          (when (string-prefix-p "likewise, " desc)
            (setq desc (replace-regexp-in-string (rx bos "likewise") (nth 1 (car arguments)) desc)))
          (when short
            (push (list short desc long) arguments))
          (when long
            (push (list long desc short) arguments))))
      ;; Similar to the likewise form described above, the argument
      ;; will sometimes use a form "like -<arg>, but" which refernces
      ;; another argument description. We want to substitute this from
      ;; with the actual descrption of the other argument.
      (cl-loop for (key desc alt) in arguments
               if (string-match (rx bos (group-n 1 "like " (group-n 2 "-" word)) ",") desc)
               collect (list key
                             (let ((like (match-string 1 desc))
                                   (arg (match-string 2 desc)))
                               (replace-regexp-in-string like (nth 1 (assoc arg arguments)) desc))
                             alt)
               else
               collect (list key desc alt)))))

(defun company-arguments--post-completion (cand)
  "remove the argument specifier from the end of the CAND.
This would include something like \"=BLOCK\" or \"[=COLOR]\"."
  (when (string-match (rx (opt (group "[")) "=" (1+ nonl) eos) cand)
    (delete-backward-char (- (length (match-string 0 cand))
                             (if (match-string 1 cand) 0 1)))))

;;;###autoload
(defun company-arguments (command &optional arg &rest ignored)
  "Complete command line arguments using --help. See `company's
COMMAND ARG and IGNORED for details."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-arguments))
    (prefix (company-arguments--prefix))
    (candidates (cons :async (lambda (callback) (company-arguments--candidates callback arg))))
    (annotation (when-let ((alt (get-text-property 0 'alternate arg)))
                  (format " (%s)" alt)))
    (meta (get-text-property 0 'description arg))
    (post-completion (company-arguments--post-completion arg))
    (sorted t)
    (ignore-case t)))

;; (let ((desc (get-text-property 0 'description arg)))
;;   (format " (%s)" (substring desc 0 (min 39 (length desc)))))

(provide 'company-arguments)
