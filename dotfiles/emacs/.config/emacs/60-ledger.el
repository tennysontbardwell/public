(defun tennyson/ledger-mode-hook ()
  (define-key ledger-mode-map (kbd "<tab>") 'completion-at-point)
  )

(add-hook 'ledger-mode-hook 'tennyson/ledger-mode-hook)


;; fix for ivy sorting
(defun cmp-length-property (a b)
  (if (< (length a) (length b)) t nil))

(defun ivy--shorter-matches-first (_name cands)
  "Sort CANDS according to their length."
  (if (nthcdr ivy-sort-max-size cands)
      cands
    (static-if (fboundp 'value<)
        (sort cands #'cmp-length-property)
      (cl-sort (copy-sequence cands) #'< #'cmp-length-property))))
