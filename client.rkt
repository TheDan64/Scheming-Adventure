#lang racket
; client.rkt by Daniel Kolsoi

(require racket/tcp)
(require "game.rkt")

(define game-world create-game-world) ; from game.rkt
(define username "") ; will be overwritten initially

; defines the tcp input and output ports
;(define-values (in out) (tcp-connect "25.7.77.62" 65525))
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

; Loop the getting of command line input from the client
(thread (let client-input ()
          (display "Scheming Adventure: ")
          (define input (read))
          
          ; the client wishes to disconnect
          (cond ((eq? input 'quit)
                 (send (list 'disconnect username))
                 (exit))
          
          (client-input))))

; Client main loop to get data from the server
(let server-input ()
  
  ; should be threaded: ?
  (define input (read in))
  
  ;(displayln input) ;tmp
  
  (cond ((not (eof-object? input))
         (cond ((eq? (car input) 'disconnect)
                (displayln (string-append (car (cdr input)) " has disconnected!")))
               
               ((eq? (car input) 'connect)
                (if (equal? (string->symbol (second input)) username) (displayln "You have connected to the server!")
                    (displayln (string-append (second input) " has connected!"))))
                    
               ((eq? (car input) 'update-gamestate) ((game-world 'set-all-info) (second input)) (displayln "TMP - Gamestate successfully received.")))
         
         (server-input))))

; cleanup on exit
(send (list 'disconnect username))
(close-input-port in)
(close-output-port out)