#lang codespells

(provide mod-name pak-folder main.rkt)

(require racket/runtime-path)

(define
  mod-name
  "Nomic")

(define-runtime-path
  pak-folder
  "BuildUnreal/WindowsNoEditor/Nomic/Content/Paks/")

(define-runtime-path
  main.rkt
  "main.rkt")

(setup-mod mod-name pak-folder)
