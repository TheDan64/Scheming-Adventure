#lang racket
; client.rkt by Daniel Kolsoi

(require racket/tcp)
(require "game.rkt")

(define game-world create-game-world) ; from game.rkt
(define username "") ; will be overwritten initially

; defines the tcp input and output ports
(define-values (in out) (tcp-connect "localhost" 65525))

; send a message to the server wrapper
(define (send msg)
  (write msg out)
  (flush-output out))

; Ask the player for a username
(define (get-username)
  (displayln "What would you like your username to be?")
  (set! username (read))
  (cond ((eq? username eof) (error "Invalid username"))))

; Tell the server you are connecting with your username
(get-username)
(send (list 'init-connect username))

; Client main loop
(let client ()
  
  ; should be threaded: ?
  (define server-input (read in))
  
  (displayln server-input) ;tmp
  
  (cond ((not (eof-object? server-input))
         (cond ((eq? (car server-input) 'disconnect)
                (displayln (string-append (car (cdr server-input)) " has disconnected!")))
               
               ((eq? (car server-input) 'connect)
                (if (equal? (string->symbol (second server-input)) username) (displayln "You have connected to the server!")
                    (displayln (string-append (second server-input) " has connected!"))))
                    
               ((eq? (car server-input) 'update-gamestate) ((game-world 'set-all-info) (second server-input)) (displayln "TMP - Gamestate successfully received.")))
         
         (client))))

; cleanup on exit
(send (list 'disconnect username))
(close-input-port in)
(close-output-port out)