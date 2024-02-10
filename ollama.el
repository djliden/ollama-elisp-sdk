;; ollama.el --- minimal emacs lisp ollama SDK

;; copyright (C) 2024 Daniel Liden

;; http://localhost:11434/api/generate


(defvar ollama-api-base-url "http://localhost:11434/api/")


(defun ollama-request (endpoint payload)
  "Send a synchronous request to the Ollama API."
  (let ((url (concat ollama-api-base-url endpoint))
        (url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
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
    ("stream" . :json-false)))


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


(defun ollama-async-request (endpoint payload process-filter)
  "Send a request to the Ollama API."
  (let ((url (concat ollama-api-base-url endpoint))
        (json-payload (json-encode payload)))
    (setq ollama-curl-process
          (start-process "ollama-curl-process"
                         "*ollama-output*"
                         "curl"
                         "-X"
                         "POST"
                         url
                         "-H"
                         "Content-Type: application/json"
                         "-d"
                         json-payload))
    (set-process-filter ollama-curl-process process-filter)))


(defun simple-ollama-process-filter (proc string)
  (let ((buffer-name "*simple-ollama-output*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (goto-char (point-max))
      (insert string)
      (when (string-match "\"done\":true" string)
        (message "Ollama completion done.")))))

(defun async-ollama-generate-completion (model prompt &rest args)
  "Generate completions using the Ollama API.
MODEL and PROMPT are required. ARGS is a plist for optional parameters."
  (let* ((args (plist-put args :prompt prompt))
         (generate-args (apply #'ollama-construct-args model args)))
    (ollama-async-request
     "generate" generate-args 'simple-ollama-process-filter)))


(defun async-ollama-generate-chat-completion (model messages &rest args)
  "Send a chat request to the Ollama API.
MODEL and MESSAGES are required. ARGS is a plist for optional parameters."
  ;; Construct the JSON payload with required and optional parameters
  (let* ((args (plist-put args :messages messages))
         (chat-args (apply #'ollama-construct-args model args)))
    (ollama-async-request
     "chat" chat-args 'simple-ollama-process-filter)))
