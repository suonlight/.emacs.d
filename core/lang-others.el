(use-package yaml-mode
  :mode "\\.ya?ml$")

(setq plantuml-jar-path "~/org-modes/plantuml.jar")
(use-package plantuml-mode
  :commands plantuml-mode)

(use-package apib-mode
  :mode ("\\.apib\\'" . apib-mode))
