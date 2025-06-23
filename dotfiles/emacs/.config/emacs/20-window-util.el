(defun tennyson//flatten-horizontal-splits (node)
  "Recursively flatten horizontal splits, returning a list of top-level columns."
  (if (windowp node)
      ;; It's a window - return it as a single column
      (list node)
    ;; It's a split
    (let ((is-horizontal (not (car node)))
          (children (cddr node)))
      (if is-horizontal
          ;; Horizontal split - flatten its children
          (apply #'append
                 (mapcar #'tennyson//flatten-horizontal-splits children))
        ;; Vertical split or other - treat as a single column
        (list node)))))

(defun tennyson//get-leftmost-window (node)
  "Get the leftmost window from a window tree node."
  (if (windowp node)
      node
    (tennyson//get-leftmost-window (car (cddr node)))))

;; Simple tree printer
(defun tennyson/show-window-tree ()
  "Display the current window tree structure."
  (interactive)
  (with-output-to-temp-buffer "*Window Tree*"
    (princ (pp-to-string (window-tree)))))

;; More readable tree display
(defun tennyson/describe-window-tree ()
  "Show a readable description of the window tree."
  (interactive)
  (message "%s" (tennyson//format-window-tree (car (window-tree)) 0)))

(defun tennyson//format-window-tree (node level)
  "Format window tree node for display."
  (let ((indent (make-string (* level 2) ?\s)))
    (if (windowp node)
        (format "%sWindow: %s" indent (buffer-name (window-buffer node)))
      (let ((split-type (if (car node) "vertical" "horizontal"))
            (children (cddr node)))
        (concat
         (format "%s%s split:\n" indent split-type)
         (mapconcat
          (lambda (child) (tennyson//format-window-tree child (1+ level)))
          children "\n"))))))

;; https://kagi.com/assistant/3791b1bd-b245-47b8-a86b-d8667cf48425
(defun tennyson/balance-windows-horizontally ()
  "Balance top-level columns, flattening nested horizontal splits into the root split."
  (interactive)
  (let* ((tree (car (window-tree)))
         (flattened-columns (tennyson//flatten-horizontal-splits tree)))
    (if (> (length flattened-columns) 1)
        ;; We have multiple columns to balance
        (let* ((num-columns (length flattened-columns))
               (total-width (frame-width))
               (target-width (/ total-width num-columns)))
          (dolist (column flattened-columns)
            (let ((leftmost-window (tennyson//get-leftmost-window column)))
              (when leftmost-window
                (let* ((current-width (window-total-width leftmost-window))
                       (delta (- target-width current-width)))
                  (when (and (not (zerop delta))
                             (window-resizable leftmost-window delta t))
                    (condition-case nil
                        (window-resize leftmost-window delta t)
                      (error nil))))))))
      ;; Only one column or no horizontal splits - just balance everything
      (balance-windows))))
