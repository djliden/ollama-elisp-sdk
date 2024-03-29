;; -*- lexical-binding: t -*-

;; ollama.el --- minimal emacs lisp ollama SDK

;; copyright (C) 2024 Daniel Liden

(defcustom ollama-api-base-url "http://localhost:11434/api/"
  "Base URL for the Ollama API."
  :type 'string
  :group 'ollama)

(defgroup ollama nil
  "Customization group for Ollama."
  :group 'applications)


(defun ollama-request (endpoint payload)
  "Send a synchronous request to the Ollama API."
  (let ((url (concat ollama-api-base-url endpoint))
        (url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")))
        (url-request-data (json-encode payload)))
    ;; Send the request
    (with-current-buffer (url-retrieve-synchronously url)
      ;; Move to the response body
      (goto-char url-http-end-of-headers)
      (forward-line)
      ;; Parse and return the JSON response
      (let ((json-object-type 'alist))
        (json-read)))))

(defun ollama-construct-args (model &rest args)
  "Construct the argument list for Ollama API calls.
Include validation to ensure either `prompt` or `messages` is provided, not both."
  `(("model" . ,model)
    ,@
    (when (plist-get args :prompt)
      `(("prompt" . ,(plist-get args :prompt))))
    ,@
    (when (plist-get args :messages)
      `(("messages" . ,(plist-get args :messages))))
    ,@
    (when (plist-member args :system)
      `(("system" . ,(plist-get args :system))))
    ,@
    (when (plist-member args :template)
      `(("template" . ,(plist-get args :template))))
    ,@
    (when (plist-member args :context)
      `(("context" . ,(plist-get args :context))))
    ,@
    (when (plist-member args :stream)
      `(("stream" . ,(plist-get args :stream))))
    ,@
    (when (plist-member args :raw)
      `(("raw" . ,(plist-get args :raw))))
    ,@
    (when (plist-member args :format)
      `(("format" . ,(plist-get args :format))))
    ,@
    (when (plist-member args :options)
      `(("options" . ,(plist-get args :options))))
    ("stream" .
     ,(if (plist-member args :stream)
          (plist-get args :stream)
        :json-false))))

(defun ollama-generate-completion (model prompt &rest args)
  "Generate completions using the Ollama API synchronously.
MODEL and PROMPT are required. ARGS is a plist for optional parameters."
  (let* ((args (plist-put args :prompt prompt))
         (generate-args (apply #'ollama-construct-args model args)))
    (ollama-request "generate" generate-args)))

(defun ollama-generate-chat-completion (model messages &rest args)
  "Send a chat request to the Ollama API synchronously.
MODEL and MESSAGES are required. ARGS is a plist for optional parameters."
  (let* ((args (plist-put args :messages messages))
         (chat-args (apply #'ollama-construct-args model args)))
    (ollama-request "chat" chat-args)))

(defun ollama-generate-embeddings (model prompt &rest args)
  "Send an embeddings request to the Ollama API synchronously.
MODEL and PROMPT are required. ARGS is a plist for optional parameters."
  (let* ((args (plist-put args :prompt prompt))
         (embed-args (apply #'ollama-construct-args model args)))
    (ollama-request "embeddings" embed-args)))


(defvar ollama-response-buffer "*ollama-response*")

(defun ollama-store-response-callback (response)
  "Store the Ollama API response in a dedicated buffer."
  (with-current-buffer (get-buffer-create ollama-response-buffer)
    (erase-buffer)
    (insert (format "%s" response))))


(defun ollama-format-store-response-callback (response)
  "Parse Ollama API response JSON and store it in a dedicated buffer as an alist."
  (with-current-buffer (get-buffer-create ollama-response-buffer)
    (erase-buffer)
    (let ((json-response
           (json-parse-string response :object-type 'alist)))
      (prin1 json-response (current-buffer)))))


(defun ollama-async-request (endpoint payload callback)
  "Send an asynchronous request to the Ollama API using url-retrieve, returning raw JSON."
  (let* ((url (concat ollama-api-base-url endpoint))
         (request-data (json-encode payload))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data request-data))
    (url-retrieve
     url
     (lambda (status)
       ;; Move to the start of the buffer to search for the HTTP status code.
       (goto-char (point-min))
       (re-search-forward "^HTTP/.* \\([0-9]+\\)" nil t)
       (let ((code (string-to-number (match-string 1))))
         (if (= code 200)
             (progn
               (goto-char url-http-end-of-headers)
               (forward-line)
               (let ((response
                      (buffer-substring-no-properties
                       (point) (point-max))))
                 (kill-buffer) ; Clean up the buffer
                 ;; Pass the raw JSON response to the callback.
                 (funcall callback response)))
           ;; Handle the case where the HTTP status code is not 200.
           (progn
             (message "Request failed with code: %s" code)
             (kill-buffer))))) ; Ensure buffer cleanup on failure
     nil t t)))


(defun async-ollama-generate-completion (model prompt &rest args)
  "Generate completions using the Ollama API.
MODEL and PROMPT are required. ARGS is a plist for optional parameters.
An optional callback function can be provided via :callback keyword."
  ;; Check for :callback in args or use 'ollama-store-response-callback' by default
  (let ((callback
         (or (plist-get args :callback)
             #'ollama-store-response-callback)))
    (setq args (plist-put args :callback nil))
    (let ((generate-args
           (apply #'ollama-construct-args
                  model
                  (cons :prompt (cons prompt args)))))
      (ollama-async-request "generate" generate-args callback))))


(defun async-ollama-generate-chat-completion
    (model messages &rest args)
  "Send a chat request to the Ollama API.
MODEL and MESSAGES are required. ARGS is a plist for optional parameters."
  ;; Construct the JSON payload with required and optional parameters
  (let* ((args (plist-put args :messages messages))
         (chat-args (apply #'ollama-construct-args model args)))
    (ollama-async-request
     "chat" chat-args #'ollama-store-response-callback)))


(defun async-ollama-generate-embeddings (model prompt &rest args)
  "Generate completions using the Ollama API.
MODEL and PROMPT are required. ARGS is a plist for optional parameters."
  (let* ((args (plist-put args :prompt prompt))
         (generate-args (apply #'ollama-construct-args model args)))
    (ollama-async-request
     "embeddings" generate-args #'ollama-store-response-callback)))

;; List Models

(defun ollama-list-models ()
  "List offline models available through Ollama"
  (let ((url-request-method "GET")
        (url (concat ollama-api-base-url "tags")))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (let ((models
             (json-parse-buffer
              :object-type 'alist
              :array-type 'list
              :null-object nil
              :false-object nil)))
        (kill-buffer)
        models))))

;; Show model details

(defun ollama-show-model-details (name)
  "Show details of a single model."
  (let ((url-request-method "POST")
        (url (concat ollama-api-base-url "show"))
        (url-request-extra-headers
         '(("Content-Type" . "application/json")))
        (url-request-data (json-encode `(("name" . ,name)))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (let ((model-details
             (json-parse-buffer
              :object-type 'alist
              :null-object nil
              :false-object nil)))
        (kill-buffer)
        model-details))))

(defun ollama-copy-model (source destination)
  "Copies an existing model, assigning the copy a new name."
  (let ((url-request-method "POST")
        (url (concat ollama-api-base-url "copy"))
        (url-request-extra-headers
         '(("Content-Type" . "application/json")))
        (url-request-data
         (json-encode
          `(("source" . ,source) ("destination" . ,destination)))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (message "%s" (buffer-string))
      (kill-buffer)
      nil)))

(defun ollama-delete-model (name)
  "Copies an existing model, assigning the copy a new name."
  (let ((url-request-method "DELETE")
        (url (concat ollama-api-base-url "delete"))
        (url-request-extra-headers
         '(("Content-Type" . "application/json")))
        (url-request-data (json-encode `(("name" . ,name)))))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (message "%s" (buffer-string))
      (kill-buffer)
      nil)))

(defun ollama-pull-model (name &optional insecure)
  "Downloads a model from the ollama library asynchronously."
  (let* ((url (concat ollama-api-base-url "pull"))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data
          (json-encode
           `(("name" . ,name)
             ,@
             (when insecure
               '(("insecure" . t)))
             ("stream" . :json-false)))))
    (url-retrieve
     url
     (lambda (status)
       (goto-char (point-min))
       (if (re-search-forward "^HTTP/.* \\([0-9]+\\)" nil t)
           (let ((code (string-to-number (match-string 1))))
             (cond
              ((= code 200)
               (search-forward "\n\n" nil t)
               (message "Model pull successful: %s" (buffer-string)))
              (t
               (message "Model pull failed with code: %s" code))))
         (message "Model pull failed: No HTTP response code found."))
       (kill-buffer))
     nil t)))

(defun ollama-push-model (name &optional insecure)
  "Uploads a model to the ollama library asynchronously."
  (let* ((url (concat ollama-api-base-url "push"))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data
          (json-encode
           `(("name" . ,name)
             ,@
             (when insecure
               '(("insecure" . t)))
             ("stream" . :json-false)))))
    (url-retrieve
     url
     (lambda (status)
       (goto-char (point-min))
       (if (re-search-forward "^HTTP/.\* \\([0-9]+\\)" nil t)
           (let ((code (string-to-number (match-string 1))))
             (cond
              ((= code 200)
               (search-forward "\n\n" nil t)
               (message "Model push successful: %s" (buffer-string)))
              (t
               (message "Model push failed with code: %s" code))))
         (message "Model push failed: No HTTP response code found."))
       (kill-buffer))
     nil t)))
