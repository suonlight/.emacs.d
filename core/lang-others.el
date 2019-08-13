(use-package yaml-mode
  :mode "\\.ya?ml$")

(setq plantuml-jar-path "~/org-modes/plantuml.jar")
(use-package plantuml-mode
  :commands plantuml-mode
  :custom
  (plantuml-default-exec-mode 'jar))

(use-package apib-mode
  :mode ("\\.apib\\'" . apib-mode))
