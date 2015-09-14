;; ------------------------------------------------------------
;; Python / PyClient

;; On Ubuntu:
;; mkdir ~/pyenv
;; virtualenv -p /usr/bin/python3 pyclient
;; source pyclient/bin/activate
;; pip install ipython
;; pip install -i http://10.158.238.251:8081/artifactory/Python-Packages/ pyasn1 pycsp pyhamcrest

;; IPython
(if (eq system-type 'windows-nt)
    (setq
     python-shell-interpreter "F:\\Dev\\Python34\\python.exe"
     python-shell-interpreter-args "-i F:\\Dev\\Python34\\Scripts\\ipython.exe"
     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
     python-shell-completion-setup-code
     "from IPython.core.completerlib import module_completion"
     python-shell-completion-module-string-code
     "';'.join(module_completion('''%s'''))\n"
     python-shell-completion-string-code
     "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
  (setq
   python-shell-interpreter "ipython"
   python-shell-interpreter-args ""
   python-shell-prompt-regexp "In \\[[0-9]+\\]: "
   python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
   python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
   python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
   python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"))

;; virtualenv(wrapper)
(require 'pyvenv)

(defun pyclient-init-session()
  (interactive)
  (python-shell-send-string "\
import pycsp.client\n\
from pycsptest.helpers import create_folder, remove_folder\n\
s = pycsp.client.logon(\"localhost\", 22222)\n"))

(defun pyclient-create-empty-test-folder()
  (interactive)
  (python-shell-send-string "\
remove_folder(s, \'~\', \'Test\')\n\
create_folder(s, \'~\', \'Test\')\n"))

(add-hook 'python-mode-hook '(lambda ()
 (define-key inferior-python-mode-map (kbd "C-c i") 'pyclient-init-session)))
(add-hook 'python-mode-hook '(lambda ()
 (define-key inferior-python-mode-map (kbd "C-c c") 'pyclient-create-empty-test-folder)))

(defun pyclient()
  (interactive)
  (if (eq system-type 'windows-nt)
      (progn
        (pyvenv-activate "F:\\Dev\\pyenv\\pyclient")
        (setq pythonpath "F:\\Dev\\pyenv\\git\\develop\\server\\scripts\\testsupport;F:\\Dev\\pyenv\\git\\develop\\pyclient\\source\\pycsp"))
    (progn
      (pyvenv-activate "~/pyenv/pyclient")
;;      (setq pythonpath "/home/johanst/develop/server/scripts/testsupport/:/home/johanst/develop/pyclient/source/pycsp")))
      (setq pythonpath "/home/johanst/develop/server/scripts/testsupport/")))
  (setenv "PYTHONPATH" pythonpath))


;; Manual PYTHONPATH (limitation is only one directory)
(defun set-pythonpath(path)
  (interactive "DPYTHONPATH directory: ")
  (let ((abspath (expand-file-name path)))
    (setq pythonpath abspath)
    (setenv "PYTHONPATH" abspath)))

(require 'my-ccmode-settings)
(add-hook 'python-mode-hook '(lambda ()
 (define-key python-mode-map (kbd "C-c C-q") 'c-format-parentheses-contents)))
(add-hook 'python-mode-hook '(lambda () (column-marker-3 80)))

(provide 'my-python-settings)
