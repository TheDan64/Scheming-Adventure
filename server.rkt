#lang racket
; server.rkt by Daniel Kolsoi

(require racket/tcp)
(require "game.rkt")

(define game-world create-game-world) ; from game.rkt

; Here are some basic functions for the server
(define (send msg port)
  (cond ((not (port-closed? port))
         (write msg port)
         (flush-output port))
  (else (displayln "Unable to send data"))))

(define (send-all msg)
  (for-each (lambda (user) (send msg (third user))) (game-world 'get-users)))

(define (update-clients)
  (for-each (lambda (user)
              (send-all (list 'connect (first user))) ; this seems to be wrong
              (cond ((not (null? user)) (send (list 'update-gamestate (game-world 'get-all-info)) (third user))))) ; this is the problematic line
              (game-world 'get-users)))

(define (check-clients)
  (for-each (lambda (user)
              (cond ((port-closed? (third user)) ((game-world 'remove-user) user) (send-all (list 'disconnect (first user))) (displayln (string-append (first user) " has disconnected!"))))) (game-world 'get-users)))

; create the listener, which will take input sent to the server
(define listener (tcp-listen 65525))

; main loop; find clients and give them their own thread
(let server ()
  ; defines the tcp input and output ports
  
  (define-values (in out) (tcp-accept listener))
  (thread (lambda ()
            
            ; handle the client's sent info
            (define username "")
            
            (let loop ()
              ; read data
              (define get-info (read in))

              ; continue looping only until hitting eof:
              (cond ((not (eof-object? get-info))
                     ; handle different events
                     (let ((msg-type (car get-info))
                           (msg (if (null? (cdr get-info)) 'none (second get-info))))

                       (cond ((eq? msg-type 'init-connect)
                              (set! username (symbol->string msg))
                              
                              ((game-world 'add-user) username in out)
                              (displayln (string-append username " has connected!")))
                             
                             ((eq? msg-type 'movement)
                              (displayln (string-append username " moves around arbitrarily.")))
                             
                             ((eq? msg-type 'disconnect)
                              (close-input-port in)
                              (close-output-port out)
                              (check-clients))
                             
                             (else (displayln (string-append "ERROR - unrecognized message passed from client: " (symbol->string msg-type))))))

                     (check-clients) ; see if the clients are still connected
                     (update-clients) ; update all connected clients
                     (loop)))                       
              
              ; cleanup ports
              (close-output-port out))))

  ; continue looking for new clients
  (server))

; cleanup
(tcp-close listener)