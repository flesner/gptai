;;; gptai.el --- Integrate with the OpenAI API -*- lexical-binding: t; -*-

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

;;
;;     ____ ____ _____  _    ___       _
;;    / ___|  _ \_   _|/ \  |_ _|  ___| |
;;   | |  _| |_) || | / _ \  | |  / _ \ |
;;   | |_| |  __/ | |/ ___ \ | | |  __/ |
;;    \____|_|    |_/_/   \_\___(_)___|_|
;;  
;; This is intended to allow for development and programming queries into the
;; OpenAI API.  This allows for sending queries straight from Emacs directly into
;; various models of OpenAI's platform.  It is a barebones implementation of a
;;     wrapper around the API focused on achieving extensibility.

;; Configurations that are required are listed as follows:
;;
;; - Define the desired model to use (available models can be found by running
;;   gptai-list-models which will populate the gptai-models variable with the
;;   list of all available models, it will also display this list in the gptai
;;   buffer).
;;
;; - Define your OpenAI API key.
;;
;; - Optionally define keybindings for sending various queries easily.
;;
;; An example of these configurations after installing from MELPA is shown
;; below:
;;
;;   (require 'gptai)
;;   ;; set standard configurations
;;   (setq gptai-model "<MODEL-HERE>")
;;   (setq gptai-api-key "<API-KEY-HERE>")
;;   ;; set keybindings optionally
;;   (global-set-key (kbd "C-c o") 'gptai-send-query)
;;

;;; Code:

;;; Customization
(defgroup gptai nil
  "Use the openAI API."
  :prefix "gptai-"
  :group 'comm
  :link '(url-link :tag "Repository" "https://github.com/antonhibl/gptai"))

;; dependencies
(require 'url)
(require 'json)

;; default values for local variables
(defcustom gptai-base-url "https://api.openai.com/v1/completions"
  "API base url for OpenAI completions endpoint."
  :type 'string
  :group 'gptai)
(defcustom gptai-models-url "https://api.openai.com/v1/models"
  "API url for listing OpenAI models."
  :type 'string
  :group 'gptai)
(defcustom gptai-chat-url "https://api.openai.com/v1/chat/completions"
  "API url for OpenAI chat endpoint."
  :type 'string
  :group 'gptai)
(defcustom gptai-images-url "https://api.openai.com/v1/images/generations"
  "API base url for generating OpenAI images."
  :type 'string
  :group 'gptai)
(defcustom gptai-model ""
  "API Model for OpenAI."
  :type 'string
  :group 'gptai)
(defcustom gptai-api-key ""
  "API key for OpenAI."
  :type 'string
  :group 'gptai)
(defcustom gptai-temperature 0.7
  "Temperature for API requests"
  :type 'int
  :group 'gptai)
(defcustom gptai-max-tokens 5000
  "Max Tokens for API requests"
  :type 'int
  :group 'gptai)
(defvar gptai-turbo-dispatch)
(defvar url-http-end-of-headers)
(defvar url-http-response-status)
(defvar gptai-image)
(defvar gptai-images)
(defvar gptai-indn)
(defvar gptai-index)

(defcustom gptai-max-retries 4
  "How many times to retry on a transient OpenAI error (429/5xx/529)."
  :type 'integer
  :group 'gptai)

(defcustom gptai-timeout 60
  "Seconds to wait for an OpenAI response before giving up."
  :type 'integer
  :group 'gptai)

(defun gptai--error-blurb ()
  "Return a short error message from the response buffer (point at body start)."
  (condition-case nil
      (let ((resp (save-excursion (json-read))))
        (or (cdr (assoc 'message (cdr (assoc 'error resp))))
            (format "%S" resp)))
    (error (buffer-substring-no-properties
            (point) (min (point-max) (+ (point) 300))))))

(defun gptai-request (gptai-prompt)
  "Sends a request to OpenAI API and return the response.
Argument GPTAI-PROMPT is the prompt to send to the API.

Inspects the HTTP status code, retries transient errors (429/5xx/529) with
exponential backoff, and signals a clear error otherwise."
  (when (or (null gptai-api-key) (string-empty-p gptai-api-key))
    (error "OpenAI API key is not set"))

  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json; charset=utf-8")
            ("Authorization" . ,(format "Bearer %s" gptai-api-key))))
         (url-request-data
          (encode-coding-string
           (json-encode `(("model" . ,gptai-model)
                          ("prompt" . ,gptai-prompt)
                          ("temperature" . ,gptai-temperature)
                          ("max_tokens" . ,gptai-max-tokens)))
           'utf-8))
         (attempt 0)
         result)

    (while (not result)
      (setq attempt (1+ attempt))
      (message "Sending request to OpenAI API using model '%s' (attempt %d)"
               gptai-model attempt)
      (let ((buffer (url-retrieve-synchronously gptai-base-url nil 'silent gptai-timeout)))
        (unless buffer
          (error "No response from OpenAI API (timeout after %ss)" gptai-timeout))
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
                      (setq result response))))
                 ;; transient -> retry with exponential backoff
                 ((memq status '(429 500 502 503 529))
                  (if (>= attempt gptai-max-retries)
                      (error "OpenAI API still failing after %d attempts (HTTP %d): %s"
                             attempt status (gptai--error-blurb))
                    (let ((wait (expt 2 attempt)))
                      (message "OpenAI returned HTTP %d; retrying in %ds..." status wait)
                      (sleep-for wait))))
                 ;; permanent error
                 (t
                  (error "OpenAI API request failed (HTTP %d): %s"
                         status (gptai--error-blurb))))))
          (kill-buffer buffer))))
    result))

(defun gptai-send-query (gptai-prompt)
  "Sends a query to OpenAI API and insert the response text at the current point.
Argument GPTAI-PROMPT prompt."
  (interactive
   (list (read-string "Prompt: ")))
  (let ((response (gptai-request gptai-prompt)))
    (let ((text (cdr (assoc 'text (elt (cdr (assoc 'choices response)) 0)))))
      (if text
          (insert text)
        (error
         "Response doesn't contain text data")))))

(defun gptai-send-query-region ()
  "Sends query to OpenAI API using region, replace region w/ response."
  (interactive)
  (let ((gptai-prompt (if (use-region-p)
                          (buffer-substring-no-properties
                           (region-beginning)
                           (region-end))
                        (read-string "Query: "))))
    (let ((response (gptai-request gptai-prompt)))
      (let ((text (cdr (assoc 'text (elt (cdr (assoc 'choices response)) 0)))))
        (delete-region (region-beginning)
                       (region-end))
        (insert text)))))

(defun gptai-send-query-buffer (&optional buffer-name)
  "Sends a query to OpenAI API using the buffer as a prompt.
Optional argument BUFFER-NAME buffer to send."
  (interactive
   (list (read-buffer "Buffer: " (current-buffer))))
  (let ((gptai-prompt (with-current-buffer buffer-name
                        (buffer-substring
                         (point-min)
                         (point-max)))))
    (let ((response (gptai-request gptai-prompt)))
      (let ((text (cdr (assoc 'text (elt (cdr (assoc 'choices response))
                                         0)))))
        (with-current-buffer buffer-name
          (erase-buffer)
          (insert text)
          (display-buffer (current-buffer) t))))))

(defun gptai-spellcheck-region ()
  "Sends query to OpenAI API to spellcheck the region."
  (interactive)
  (let ((gptai-prompt (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "Text: "))))
    (with-current-buffer (current-buffer)
      (let ((response (gptai-request (format "Spellcheck this text: %s" gptai-prompt))))
        (let ((text (cdr (assoc 'text (elt (cdr (assoc 'choices response)) 0)))))
          (delete-region (region-beginning) (region-end))
          (insert text))))))

(defun gptai-elaborate-on-region ()
  "Sends query to OpenAI API to elaborate on the region."
  (interactive)
  (let ((gptai-prompt (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "Text: "))))
    (with-current-buffer (current-buffer)
      (let ((response (gptai-request (format "Elaborate on this text: %s" gptai-prompt))))
        (let ((text (cdr (assoc 'text (elt (cdr (assoc 'choices response)) 0)))))
          (delete-region (region-beginning) (region-end))
          (insert text))))))

(defun gptai-code-query-region (gptai-language)
  "Sends instructions to OpenAI API to code in a language.
Argument GPTAI-INSTRUCTIONS code instructions for query.
Argument GPTAI-LANGUAGE language to generate."
  (interactive
   (list (read-string "Language: ")))
  (let ((gptai-code-prompt (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "Code: "))))
    (with-current-buffer (current-buffer)
      (let ((response (gptai-request (format "%s(%s)" gptai-code-prompt gptai-language))))
        (let ((text (cdr (assoc 'text (elt (cdr (assoc 'choices response)) 0)))))
          (delete-region (region-beginning) (region-end))
          (insert text))))))

(defun gptai-code-query (gptai-instructions gptai-language)
  "Sends instructions to OpenAI API to code in a language.
Argument GPTAI-INSTRUCTIONS code instructions for query.
Argument GPTAI-LANGUAGE language to generate."
  (interactive
   (list (read-string "Instructions: ")
         (read-string "Language: ")))
  ;; place code in current buffer
    (with-current-buffer (current-buffer)
      (let ((response (gptai-request (format "%s(%s)" gptai-instructions gptai-language))))
        (let ((text (cdr (assoc 'text (elt (cdr (assoc 'choices response)) 0)))))
          (delete-region (region-beginning) (region-end))
          (insert text)))))

(defun gptai-explain-code-region ()
  "Sends region to OpenAI API to explain."
  (interactive)
  (let ((gptai-prompt (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "Code: "))))
    (with-current-buffer (current-buffer)
      (let ((response (gptai-request (format "Explain the following: %s" gptai-prompt))))
        (let ((text (cdr (assoc 'text (elt (cdr (assoc 'choices response)) 0)))))
          (insert text)
          (insert "\n\n"))))))

(defun gptai-document-code-region ()
  "Sends region to OpenAI API to write documentation."
  (interactive)
  (let ((gptai-prompt (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "Code: "))))
    (with-current-buffer (current-buffer)
      (let ((response (gptai-request (format "Please write the documentation for the following code: %s" gptai-prompt))))
        (let ((text (cdr (assoc 'text (elt (cdr (assoc 'choices response)) 0)))))
          (insert text)
          (insert "\n\n"))))))

(defun gptai-optimize-code-region ()
  "Sends region to OpenAI API to optimize."
  (interactive)
  (let ((gptai-prompt (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "Code: "))))
    (with-current-buffer (current-buffer)
      (let ((response (gptai-request (format "Optimize and refactor the following code: %s" gptai-prompt))))
        (let ((text (cdr (assoc 'text (elt (cdr (assoc 'choices response)) 0)))))
          (delete-region (region-beginning) (region-end))
          (insert text))))))


(defun gptai-improve-code-region ()
  "Sends region to OpenAI API to improve."
  (interactive)
  (let ((gptai-prompt (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "Code: "))))
    (with-current-buffer (current-buffer)
      (let ((response (gptai-request (format "Improve and extend the following code: %s" gptai-prompt))))
        (let ((text (cdr (assoc 'text (elt (cdr (assoc 'choices response)) 0)))))
          (delete-region (region-beginning) (region-end))
          (insert text))))))

(defun gptai-fix-code-region ()
  "Sends region to OpenAI API to fix a bug."
  (interactive)
  (let ((gptai-prompt (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "Code: "))))
    (with-current-buffer (current-buffer)
      (let ((response (gptai-request (format "There is a bug in the following function, please help me fix it: %s" gptai-prompt))))
        (let ((text (cdr (assoc 'text (elt (cdr (assoc 'choices response)) 0)))))
          (delete-region (region-beginning) (region-end))
          (insert text))))))

(defun gptai-send-image-query (prompt n size filepath)
  "Sends a query to the OpenAI Image Generation API.
Argument PROMPT prompt to send.
Argument N number of images to generate.
Argument SIZE size of images.
Argument FILEPATH filepath to download to."
  (interactive
   (list (read-string "Prompt: ")
         (read-number "Number of images to generate: " 1)
         (read-string "Image size (e.g. 1024x1024): " "1024x1024")
         (read-directory-name "Enter output directory: " "~/Pictures")))
  (when (null gptai-api-key)
    (error "OpenAI API key is not set"))
  (let* ((url (custom-value 'gptai-images-url))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s" gptai-api-key))))
         (url-request-data
          (json-encode `(("prompt" . ,prompt)
                         ("n" . ,n)
                         ("size" . ,size)))))
    (message "Sending request to OpenAI Image Generation API")
    (condition-case err
        (with-current-buffer
            (url-retrieve-synchronously url nil 'silent)
          (goto-char url-http-end-of-headers)
          (let ((response (json-read)))
            (when (assoc 'error response)
              (error (cdr (assoc 'message (cdr (assoc 'error response))))))
            (with-current-buffer (get-buffer-create "*openai*")
              (erase-buffer)
              (insert (format "Generated image URLs:\n"))
              (setq gptai-indn n)
              (setq gptai-image (let ((gptai-indn (length (cdr (assoc 'data response))))
                    (urls '()))
                (dotimes (x gptai-indn)
                  (push (cdr (assoc 'url (elt (cdr (assoc 'data response)) x))) urls))
                (reverse urls)))
              (insert (format "%s\n"
                              (let ((gptai-indn (length (cdr (assoc 'data response))))
                                    (urls '()))
                                (dotimes (x gptai-indn)
                                  (push (cdr (assoc 'url (elt (cdr (assoc 'data response)) x))) urls))
                                (reverse urls)))))
            (switch-to-buffer-other-window (current-buffer)))
          (setq gptai-index 0)
          (let ((gptai-images gptai-image))
              (dolist (gptai-image gptai-images)
                (async-shell-command (format "curl '%s' > %s/%s_%d.png" gptai-image filepath (format-time-string "%T") gptai-index))
                (sleep-for 2)
                (setq gptai-index (+ gptai-index 1))))
            (message "Finished downloading %d images to %s" n filepath))
      (error (error "Error while sending request to OpenAI Image Generation API: %s"
                    (error-message-string err))))))

(defun gptai-list-models ()
  "Retrieves a list of currently available GPT-3 models from OpenAI."
  (interactive)
  (with-current-buffer (get-buffer-create "*gptai-models*")
    (erase-buffer)
    (async-shell-command (format "curl %s \
    -H 'Authorization: Bearer %s'" gptai-models-url gptai-api-key) "*gptai-models*" "*Messages*")
    (goto-char (point-min))
    (re-search-forward "^.*object.*$")
    (delete-region (point-min) (point))
    (pop-to-buffer (current-buffer))))

(provide 'gptai)
;;; gptai.el ends here
