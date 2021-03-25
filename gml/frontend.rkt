#lang at-exp racket

(provide serve-renderer
         (all-from-out nomic/gml/frontend/client/views))

(require nomic/gml/frontend/client/views)
(require racket-react/client
         racket/runtime-path)

(define-runtime-path my-app
  "frontend\\client\\my-app")

(react-app-folder my-app)

(define (serve-renderer g)
  (displayln (compile-app components))

  
  (save-app)

 

  #;
  (when (not (system @~a{netstat -ano | find "3000"}))
    
    (system (~a "cd " (~a (react-app-folder))
                "&& npm start"
                )))
  )