;; ollama.el --- minimal emacs lisp ollama SDK

;; copyright (C) 2024 Daniel Liden

;; http://localhost:11434/api/generate


(defvar ollama-api-base-url "http://localhost:11434/api/")

(defun ollama-send-request (endpoint payload process-filter)
  "Send a request to the Ollama API."
  (let ((url (concat ollama-api-base-url endpoint))
        (json-payload (json-encode payload)))
    (setq ollama-curl-process
          (start-process "ollama-curl-process" "*ollama-output*"
                         "curl" "-X" "POST" url "-H" "Content-Type: application/json" "-d" json-payload))
    (set-process-filter ollama-curl-process process-filter)))


(defun simple-ollama-process-filter (proc string)
  (let ((buffer-name "*simple-ollama-output*"))
    (with-current-buffer (get-buffer-create buffer-name)
      (goto-char (point-max))
      (insert string)
      (when (string-match "\"done\":true" string)
        (message "Ollama completion done.")))))

