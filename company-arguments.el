;;; company-arguments.el --- company backend for command line arguments  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Troy Hinckley

;; Author: Troy Hinckley <troy.hinckley@gmail.com>
;; URL: https://github.com/CeleritasCelery/company-async-files
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

;; Company backend for completing command line arguments.
;; =company-async-files= provides the same completion as =company-files=,
;; but asynchronously uses find in the background to get the candidates.
;; This ensures that your user thread is never blocked by your completion
;; backend, which is the way it should be.

;;; Code:

(require 'company)
(require 'dash)
(require 'cl-lib)
(require 'ample-regexps)

(defun company-arguments--get-command ()
  (save-excursion
    (back-to-indentation)
    (buffer-substring (point)
                      (1- (re-search-forward (rx (or space eol)))))))

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
  (let ((command (company-arguments--get-command))
        (buffer (generate-new-buffer "company-arguments-command-output")))
    (set-process-sentinel (start-process "company-argments-candidates"
                                         buffer
                                         (locate-file command (split-string (getenv "PATH") ":"))
                                         "--help")
                          (lambda (_ event)
                            (when (equal event "finished\n")
                              (funcall callback
                                       (seq-sort-by
                                        'length '<
                                        (cl-loop for (arg desc alt) in (company-arguments--parse-help buffer)
                                                 when (string-prefix-p prefix arg)
                                                 collect (progn (put-text-property 0 1 'description desc arg)
                                                                (put-text-property 0 1 'alternate alt arg)
                                                                arg))))
                              (kill-buffer buffer))))))


(define-arx arg-rx
  '((spc (1+ space))
    (short (group-n 1 "-" word))
    (long (group-n 2 "--"
                   (1+ (any word "_-"))
                   (optional "[")
                   (optional "=" (1+ word))
                   (optional "]")))))

(let ((buffer (get-buffer "pipe")))
  (company-arguments--parse-help buffer))

(defun company-arguments--parse-help (buffer)
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
              (rest (cl-loop initially (forward-line)
                             while (looking-at (rx bol (>= 7 space)
                                                   (group-n 1 (1+ nonl))))
                             collect (match-string 1)
                             do (forward-line))))
          (setq desc (mapconcat 'identity (cons desc rest) " "))
          (when (string-prefix-p "likewise, " desc)
            (setq desc (replace-regexp-in-string (rx bos "likewise") (nth 1 (car arguments)) desc)))
          (when short
            (push (list short desc long) arguments))
          (when long
            (push (list long desc short) arguments))))
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
  (when (string-match (rx (opt (group "[")) "=" (1+ nonl) eos) cand)
    (delete-backward-char (- (length (match-string 0 cand))
                             (if (match-string 1 cand) 0 1)))))

;;;###autoload
(defun company-arguments (command &optional arg &rest ignored)
  "Complete file paths using find. See `company's COMMAND ARG and IGNORED for details."
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
