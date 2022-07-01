(defcustom tlt-OCLC-biber t 
  "Whether or not to remove OCLCs from your `.bib' file on every biber run."
  :group 'tlt-utils)

(defcustom tlt-OCLC-org nil 
  "Whether or not to remove OCLCs from your `.bib' file on every org export."
  :group 'tlt-utils)

(defun tlt-remove-OCLC-from-entry (key beg end)
  "Remove all OCLC entries in the annotation field of the current entry.
  If the only entry of that annotation field is the OCLC, remove the
  whole field."
  (let ((bibtex-dialect 'BibTeX)  
        (annot (bibtex-autokey-get-field "annotation"))                    ; call the annotation field string "annots"
                                        ; to avoid error "stringp, nil"
        (OCLC "\s?O?CLC:[^,}]+"))                                          ; and the OCLC string "OCLC" ("O?" if OCLC first item, see `looking-at')

    (when (string-match OCLC annot)                                        ; if an OCLC is in that annotation string
      (progn                                                               ; do the following (else stop and return nil):
        (setq annot (replace-match "" nil nil annot)))                     ; remove the annotation string

      (when (string-match "^,\s?\\|\s?,$" annot)                           ; if there is a comma left at the beginning or end
        (setq annot (replace-match "" nil nil annot)))                     ; remove it, too

      (when (string-match ",," annot)                                      ; and if a comma occurs twice successively
        (setq annot (replace-match "," nil nil annot)))                    ; replace it by a single occurrence

      (bibtex-beginning-of-entry)                                          ; go to the beginning of the entry, e.g. "@book KEY"
      (goto-char (car (cdr (bibtex-search-forward-field "annotation" t)))) ; go right before the annotation field
      (if (looking-at (format "{[^}]%s\s*}" OCLC))                         ; if you're looking at just the OCLC in curly brackets
          (bibtex-kill-field)                                              ; remove the whole field
        (bibtex-kill-field)                                                ; else also remove the whole field
        (bibtex-make-field "annotation")                                   ; but in addition make a new annotation field,
        (backward-char)                                                    ; go inside of the curly brackets
        (insert annot)))))

(defun tlt-remove-OCLCs-buffer ()
  "Remove all OCLCs in the annotation fields of the `bibtex' file 
in the current buffer. 
  If the only entry of that annotation field is the OCLC, remove the
  whole field.
Loops over `tlt-remove-OCLC-from-entry'."
  (interactive)
  (bibtex-set-dialect 'BibTeX t)
  (bibtex-map-entries #'tlt-remove-OCLC-from-entry))

(defun tlt-remove-OCLCs-file (filename)
  "Remove all OCLCs in the annotation fields of a specified .bib FILE. 
  If the only entry of that annotation field is the OCLC, remove the
  whole field."
  (with-temp-buffer                                     ; in a temporary buffer,
    (insert-file filename)                              ; insert the contents of FILE
    (tlt-remove-OCLCs-buffer)                           ; remove the OCLCs in there
    (write-file filename)))                              ; and save the changes

(defun tlt-get-bibliography-files-buffer (&optional backend)
  "Find the bibliography files relevant for compiling 
    the LaTeX document in the current buffer. With optional
  argument, find those bibliography files for FILE. Mostly
  copied from `reftex-locate-bibliography-files'."
  (let ((files))                            ; temporarily create the empty variable `file'
    (goto-char (point-min))                 ; go the the beginning of the buffer
    (while 		                          ; ans as long as
        (re-search-forward                  ; searching forward
         (concat                            ; one of
          "\\(^\\)[^%\n\r]*\\\\\\("         ; the `reftex-bibliography-commands'
          (mapconcat #'identity 
                     '("bibliography" "nobibliography" "addbibresource") "\\|") ; is found
          "\\)\\(\\[.+?\\]\\)?{[ \t]*\\([^}]+\\)")
         nil t)                             ;  show error messages but return nil instead
      (setq files                           ; set the files 
            (append files                   ; to be a list to which the results is appened
                    (split-string (reftex-match-string 4) ; whilst the irrelevant parts of the match
                                  "[ \t\n\r]*,[ \t\n\r]*")))) ; are deleted
    (delete-dups files)))                 ; and return the list of these files, removing duplicates

(defun tlt-remove-OCLCs-auto (&optional backend subtreep visible-only body-only ext-plist)
  (interactive)
  "Automatically find the bibliography files relevant for compiling 
   the LaTeX document at hand and remove all OCLC entries from them."
  (mapc #'tlt-remove-OCLCs-file 
        (tlt-get-bibliography-files-buffer)))

(defun tlt-OCLC-add-advice ()
  "Activate automatic removal of OCLCs."
  (when tlt-OCLC-biber
    (advice-add 'TeX-run-BibTeX :before #'tlt-remove-OCLCs-auto) 
    (advice-add 'TeX-run-Biber  :before #'tlt-remove-OCLCs-auto))
  (when tlt-OCLC-org
    (add-hook 'org-export-before-parsing-hook #'tlt-remove-OCLCs-auto)))

(defun tlt-OCLC-remove-advice ()
  "Activate automatic removal of OCLCs."
  (advice-remove 'TeX-run-BibTeX #'tlt-remove-OCLCs-auto) 

  (advice-remove 'TeX-run-Biber #'tlt-remove-OCLCs-auto)

  (remove-hook 'org-export-before-parsing-hook #'tlt-remove-OCLCs-auto))

(provide 'tlt-remove-OCLC)

;; tlt-remove-OCLC ends here ;;
