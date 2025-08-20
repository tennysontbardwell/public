(defun tennyson/run-ai-on-region-with-instructions (instructions &optional model)
  "Write the current region to a temporary file and run 'foo bar -f FILE' asynchronously.
Display the results in a new buffer."
  (interactive)
  (unless (region-active-p)
    (error "No active region"))

  (let* ((region-content (buffer-substring-no-properties (region-beginning) (region-end)))
         (temp-file (make-temp-file "/tmp/emacs-tt-ai-input" nil ".tmp"))
         (output-buffer-name "*foo-bar-output*")
         (model (or model "o4-mini"))
         (process-name "foo-bar-process"))

    ;; Write region content to temporary file
    (with-temp-file temp-file
      (insert region-content))

    ;; Create or clear the output buffer
    (with-current-buffer (get-buffer-create output-buffer-name)
      (erase-buffer)
      (insert (format "Running: tt ai with -f %s\n" temp-file))
      (insert "=" (make-string 50 ?=) "\n\n"))

    ;; Display the output buffer
    (display-buffer output-buffer-name)

    ;; Start the async process
    (let ((process (start-process process-name output-buffer-name
                                  "tt" "ai" instructions "-f" temp-file)))

      ;; Set up process sentinel to handle completion and cleanup
      (set-process-sentinel
       process
       (lambda (proc event)
         (when (memq (process-status proc) '(exit signal))
           (with-current-buffer (process-buffer proc)
             (goto-char (point-max))
             (insert (format "\n\nProcess %s %s"
                             (process-name proc)
                             (if (string= event "finished\n")
                                 "completed successfully"
                               (format "exited with: %s" (string-trim event)))))
             ;; Clean up temporary file
             (when (file-exists-p temp-file)
               (delete-file temp-file))
             (insert (format "\nTemporary file %s cleaned up." temp-file)))))

       ;; Optional: Set up filter to handle real-time output
       (set-process-filter
        process
        (lambda (proc string)
          (when (buffer-live-p (process-buffer proc))
            (with-current-buffer (process-buffer proc)
              (goto-char (point-max))
              (insert string)))))

       (message "Started foo bar process on region. Output in buffer: %s" output-buffer-name)))))

(defun tennyson/ai-proofread ()
  (interactive)
  (tennyson/run-ai-on-region-with-instructions
   "Proof read this and suggest minor corrections"
   "o4-mini"))

(defun tennyson/ai-coderewrite ()
  (interactive)
  (tennyson/run-ai-on-region-with-instructions
   "Read the following code snippet and make the requested changes. Be terse and reduce redundant code. Output only the replacement code that should overwrite the input segment. Include excess information as terse comments."
   "o3"))
