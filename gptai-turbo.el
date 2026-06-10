;;; gptai-turbo.el --- Integrate with the gpt-3.5-turbo model -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Hibl, Anton

;; Author: Anton Hibl <antonhibl11@gmail.com>
;; URL: https://github.com/antonhibl/gptai
;; Keywords: comm, convenience
;; Version: 1.0.5
;; Package-Requires: ((emacs "24.1"))

;; This program is free software; you can redistribute it and/or modify
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

;; A rough start to integrating the newer chat endpoints into Emacs, working
;; towards a larger chat functionality for this package and the editor as a
;; whole.

;;; Code:

(require 'url)
(require 'gptai)

(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defcustom gptai-turbo-max-retries 4
  "How many times to retry on a transient OpenAI error (429/5xx/529)."
  :type 'integer
  :group 'gptai)

(defcustom gptai-turbo-timeout 60
  "Seconds to wait for an OpenAI response before giving up."
  :type 'integer
  :group 'gptai)

(defun gptai--turbo-error-blurb ()
  "Return a short error message from the response buffer (point at body start)."
  (condition-case nil
      (let ((resp (save-excursion (json-read))))
        (or (cdr (assoc 'message (cdr (assoc 'error resp))))
            (format "%S" resp)))
    (error (buffer-substring-no-properties
            (point) (min (point-max) (+ (point) 300))))))

;;;###autoload
(defun gptai-turbo-request (gptai-prompt)
  "Sends a request to OpenAI API's turbo endpoint and return the response.
Argument GPTAI-PROMPT is the prompt to send to the API.

Inspects the HTTP status code, retries transient errors (429/5xx/529) with
exponential backoff, and signals a clear error otherwise."
  (when (or (null gptai-api-key) (string-empty-p gptai-api-key))
    (error "OpenAI API key is not set"))

  (let* ((url "https://api.openai.com/v1/chat/completions")
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json; charset=utf-8")
            ("Authorization" . ,(format "Bearer %s" gptai-api-key))))
         (url-request-data
          (encode-coding-string
           (json-encode `(("model" . ,gptai-model)
                          ("messages" . [((role . "user") (content . ,gptai-prompt))])
                          ("temperature" . ,gptai-temperature)))
           'utf-8))
         (attempt 0)
         result)

    (while (not result)
      (setq attempt (1+ attempt))
      (message "Sending request to OpenAI API using model '%s' (attempt %d)"
               gptai-model attempt)
      (let ((buffer (url-retrieve-synchronously url nil 'silent gptai-turbo-timeout)))
        (unless buffer
          (error "No response from OpenAI API (timeout after %ss)" gptai-turbo-timeout))
        (unwind-protect
            (with-current-buffer buffer
              (let ((status (or url-http-response-status 0)))
                (goto-char (or url-http-end-of-headers (point-min)))
                (cond
                 ;; success
                 ((and (>= status 200) (< status 300))
                  (let ((response (json-read)))
                    (if (assoc 'error response)
                        (error "OpenAI API error: %s"
                               (cdr (assoc 'message (cdr (assoc 'error response)))))
                      (let ((first-choice (elt (cdr (assoc 'choices response)) 0)))
                        (setq result
                              (cdr (assoc 'content
                                          (cdr (assoc 'message first-choice)))))))))
                 ;; transient -> retry with exponential backoff
                 ((memq status '(429 500 502 503 529))
                  (if (>= attempt gptai-turbo-max-retries)
                      (error "OpenAI API still failing after %d attempts (HTTP %d): %s"
                             attempt status (gptai--turbo-error-blurb))
                    (let ((wait (expt 2 attempt)))
                      (message "OpenAI returned HTTP %d; retrying in %ds..." status wait)
                      (sleep-for wait))))
                 ;; permanent error
                 (t
                  (error "OpenAI API request failed (HTTP %d): %s"
                         status (gptai--turbo-error-blurb))))))
          (kill-buffer buffer))))
    result))

;;;###autoload
(defun gptai-turbo-query (gptai-prompt)
  "Sends a request to turbo and insert response at the current point.
Argument GPTAI-PROMPT prompt to be sent."
  (interactive "sEnter your prompt: ")
  (let ((response (gptai-turbo-request gptai-prompt)))
    (insert response)))

;;;###autoload
(defun gptai-turbo-query-region (start end)
  "Sends a request to turbo using region and replace region w/ response at the current point."
  (interactive "r")
  (let* ((region-text (buffer-substring-no-properties start end))
         (response (gptai-turbo-request region-text)))
    (insert (concat "
" response))))

(provide 'gptai-turbo)
;;; gptai-turbo.el ends here
