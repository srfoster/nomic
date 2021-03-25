#lang at-exp codespells

(require "./nomic-app/controllers.rkt"
         "./lang.rkt"
         "./base.rkt"
         nomic/mod-info
         nomic/spells)

(require-mod dev-runes)
(require-mod fire-particles)
(require-mod ice-particles)

;(at [-4921.04248 -6382.978516 15592.59375] (nexus #:mana 100))

(define my-mod-lang
  (append-rune-langs #:name main.rkt  
                     (dev-runes:my-mod-lang #:with-paren-runes? #t)))

(module+ main
  (codespells-server-port 7998)
  (unreal-server-port 7999)
  (codespells-workspace ;TODO: Change this to your local workspace if different
   (build-path (current-directory) ".." ".." "CodeSpellsWorkspace"))
  (extra-unreal-command-line-args "-WinX=50 -WinY=50 -ResX=1900 -ResY=1200 -Windowed")

  (start-game! "Stephen" "Lindsey")
  (thread
   start-server)
  (once-upon-a-time
   #:world  (arena-world)
   #:aether (demo-aether
             #:lang my-mod-lang)))