;; ollama.el --- minimal emacs lisp ollama SDK

;; copyright (C) 2024 Daniel Liden

;; http://localhost:11434/api/generate


(defvar ollama-api-base-url "http://localhost:11434/api/")

(defun ollama-send-request (endpoint payload process-filter)
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


(defun ollama-generate-completion (model prompt &rest args)
  "Generate completions using the Ollama API.
MODEL and PROMPT are required. ARGS is a plist for optional parameters."
  ;; Construct the JSON payload with optional parameters included if they are provided
  (let
      ((generate-args
         `(("model" . ,model)
           ("prompt" . ,prompt) ,@
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
           ;; `stream` is hardcoded as :json-false based on your requirement
           ("stream" . :json-false))))
    ;; Send the request to the Ollama API
    (ollama-send-request
     "generate" generate-args 'simple-ollama-process-filter)))



