(require 'tlt-tidy-dir)
(require 'tlt-tex-precompile)
(require 'tlt-run-biber)
(require 'tlt-compile-frame)
(require 'tlt-remove-OCLC)

(defgroup tlt-utils nil
  "General utilities helpful for logicians."
  :group 'tlt)

(defvar tlt-TeX-utils-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M b") 'org-call-biber)
    (define-key map (kbd "M-o")   'tlt-tex-switch-to-biber-output)
    map)
  "Keymap for `tlt-tex-utils'.")

(define-minor-mode tlt-tex-utils-mode
  "Minor mode with utilities to make writing and compiling `.tex' files more comfortable. 
            Provides the following utilities:
            1. Keep your folder clean by outsourcing the auxiliary files to another folder whilst
            allowing these files to be reused. See `tlt-utils-box-up-aux-files' and `tlt-utils-unbox-aux-files'.
            2. Detect the bibliography files used in the current file and remove their OCLCs. See `tlt-remove-OCLCs-auto'.
            3. Allow biber to be run from your `.org' file. See `tlt-org-run-biber'.
            4. Add the functionality to precompile a preamble. See `tlt-precompile-preamble'.
            5. Allow compiling single frames in org-beamer-mode. See `tlt-compile-beamer-frame'."
  :init-value nil
  (if tlt-tex-utils-mode           

      ;; if the mode is turned on ;;

      (progn                                    
        (tlt-auxiliary-files-tex-add-advice-maybe)     ; (un-)boxing in `.tex' files
        (tlt-auxiliary-files-org-add-advice-maybe)     ; (un-)boxing in `.org' files
        (tlt-OCLC-add-advice)                          ; removing OCLCs
        (tlt-tex-install-precompiling)                 ; precompiling preambles
        (advice-add 'TeX-Biber-sentinel :around        ; compiling single beamer-frame
                    #'tlt-org-biber-sentinel-wrapper)) 

    ;; if the mode is turned off ;;

    (tlt-auxiliary-files-tex-remove-advice)            
    (tlt-auxiliary-files-org-remove-advice)
    (tlt-tex-uninstall-precompiling)
    (tlt-OCLC-remove-advice)
    (advice-remove 'TeX-Biber-sentinel
                   #'tlt-org-biber-sentinel-wrapper) 
    ))

(provide 'tlt-tex-utils)

;; tlt-tex-utils.el ends here ;;
