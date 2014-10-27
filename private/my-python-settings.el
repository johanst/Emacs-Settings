;; ------------------------------------------------------------
;; Python / PyClient

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

;; virtualenv
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(if (eq system-type 'windows-nt)
    (setq venv-location "F:\\Dev\\pyenv\\")
  (setq venv-location "~/pyenv/"))

(provide 'my-python-settings)
