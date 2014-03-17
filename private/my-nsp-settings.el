;; ------------------------------------------------------------
;; Nsp specific build stuff

(setq nsp-branches-alist '(("INT-Server" . "/home/johanst/INT-Server/server")
                           ("INT-V140-SP1" . "/home/johanst/INT-V140-SP1/packages/commons/components/C3Po")
                           ("RB-1.3" . "/home/johanst/RB-1.3/packages/commons/components/C3Po")
                           ("RB-1.4" . "/home/johanst/RB-1.4/packages/commons/components/C3Po")
                           ))

(defun nsp-add-branch(branch-name server-dirname)
  (interactive "Mbranch name: \nDServer directory: ")
  (push (cons branch-name server-dirname) nsp-branches-alist))

(defvar nsp-projects-alist nil)
(defvar nsp-current-branch nil)
(defvar nsp-current-project "nsp_servers")

(defun nsp-update-tags()
  (interactive)
  (when nsp-current-branch
    (cd (car compilation-search-path))
    (shell-command "etags `find . -name \"*.c\" -o -name \"*.cpp\" -o -name \"*.h\"`")))

(defun nsp-update-mode-string()
  (setq global-mode-string
        (concat nsp-current-branch " / " nsp-current-project)))

(defun nsp-select-branch()
  (interactive)
  (let* ((branch-name (ido-completing-read
                       "Select branch: "
                       (mapcar '(lambda (elt) (car elt)) nsp-branches-alist)))
         (server-dirname (cdr (assoc branch-name nsp-branches-alist)))
         (tags-filename (concat server-dirname "/TAGS")))
    (setq nsp-current-branch branch-name)
    (setq nsp-current-project "nsp_servers")
    (setq nsp-projects-alist nil)
    (nsp-update-mode-string)
    (setq compilation-search-path (list server-dirname))
    (unless (file-readable-p tags-filename)
      (nsp-update-tags))
    (visit-tags-table tags-filename)
    t
))

(defun nsp-get-makefiles()
  (when nsp-current-branch
    (cd (car compilation-search-path))
    (split-string
     (shell-command-to-string (concat "find -name Makefile -exec grep -q "
                                      "\"component_type\\s:=\\s\\(executable\\|component_test\\)\" "
                                      "'{}' \\; -print | sed -e \"s/^.//\""))
     "\n" t)))

(defun nsp-get-projects()
  (when nsp-current-branch
    (mapcar '(lambda (mkfile)
               (cons
                (if (member "test" (split-string mkfile "/"))
                    (concat "test_" (car (last (nbutlast (split-string mkfile "/") 2))))
                  (car (last (nbutlast (split-string mkfile "/")))))
                (replace-regexp-in-string "/Makefile" "" mkfile)))
            (nsp-get-makefiles))))

(defun nsp-select-project()
  (interactive)
  (when nsp-current-branch
    (unless nsp-projects-alist
      (setq nsp-projects-alist (nsp-get-projects)))
    (setq nsp-current-project
          (ido-completing-read
           "Select project: "
           (mapcar '(lambda (elt) (car elt)) nsp-projects-alist)))
    (nsp-update-mode-string)))

;; (nsp-get-include-guard-name
;;  "~/johanst/INT-V150/server/nsp_servers/plugins/LONPlugIn/interface/nsp/Status.h")
;; --> "TAC_NSP_LONPLUGIN_STATUS"
(defun nsp-get-include-guard-name(fullpath)
  (defun nsp-parent-directory(filepath)
    (let ((dirname (file-name-directory filepath)))
      (if dirname
          (directory-file-name dirname)
        nil)))
  (defun nsp-add-to-include-guard-names(names dirname)
    (let ((name (file-name-base dirname)))
      (cond ((not dirname) names)
            ((or (string-equal name "nsp")
                 (string-equal name "interface")
                 (string-equal name "include"))
            (nsp-add-to-include-guard-names names (nsp-parent-directory dirname)))
            ((or (string-equal name "")
                 (string-equal name "plugins")
                 (string-equal name "nsp_servers"))
             names)
            (t (nsp-add-to-include-guard-names
                (cons name names)
                (nsp-parent-directory dirname))))))
  (concat "tac_nsp_"
          (mapconcat
           'identity
           (nsp-add-to-include-guard-names
            (list (file-name-base fullpath))
            (nsp-parent-directory fullpath))
           "_")))

(nsp-get-include-guard-name "/home/johanst/extflash/INT-V150/server/nsp_servers/csc/runtimedbmanager/interface/nsp/ObjectHandle.h")

(setq c-custom-include-guard-name 'nsp-get-include-guard-name)

(condition-case nil
    (require 'my-local-nsp-branch-config)
  (error nil))

(setq ff-search-directories
      '("."
        "../../source/*"
        "../interface/*"
        "../include/*"
        "../../interface/*"
        "../../include/*"
        "../../../source/*"
        "../interface/nsp/impex/*"
        "../include/nsp/impex/*"))

(provide 'my-nsp-settings)
