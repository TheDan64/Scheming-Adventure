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

; For combining a list of strings
(define (concat lst)
  (cond ((null? lst) "")
        ((string? (car lst)) (string-append (car lst) " " (concat (cdr lst))))
        (else "")))

; Ask the player for a username
(define (get-username)
  (displayln "What would you like your username to be?")
  (set! username (read))
  (cond ((eq? username eof) (error "Invalid username"))))

; Tell the server you are connecting with your username
(get-username)
(send (list 'init-connect username))

; Display some intro text to introduce the text game
(displayln (string-append (symbol->string username) ", you find yourself in a dark forest."))
(displayln "NOTE - Type 'commands' to see a list of currently available commands and 'more-info (command)' to get additional information on using that command")

; Loop the getting of command line input from the client
(thread (let client-input ()
          ;(thread (lambda ()
          (display "Scheming Adventure: ")
          (define input (read-line))
          
          (cond ((not (null? (string-split input)))
                 ; parse some client-inputted commands
                 (let ((start (car (string-split input)))
                       (remain (cdr (string-split input))))
                   (cond ((equal? input "quit")
                          (send (list 'disconnect username))
                          (exit))
                         ((equal? input "commands")
                          (displayln "Here are the available commands:")
                          (displayln "commands, examine, look-around, players, say, quit."))
                         ((equal? input "players")
                          (displayln "These are the currently connected players:")
                          (for-each (lambda (x) (displayln x)) (game-world 'get-users)))
                         ((equal? start "more-info")
                          (cond ((null? remain) 
                                 (displayln "What would you like more info on?"))
                                ((equal? (car remain) "commands")
                                 (displayln "Usage - 'commands'"))
                                ((equal? (car remain) "players")
                                 (displayln "Usage - 'players'"))
                                ((equal? (car remain) "say")
                                 (displayln "Usage - 'say (text)'"))
                                ((equal? (car remain) "look-around")
                                 (displayln "Usage - 'look-around'"))
                                ((equal? (car remain) "examine")
                                 (displayln "Usage - 'examine (target)'"))
                                ((equal? (car remain) "quit")
                                 (displayln "Usage - 'quit'"))))
                         ((equal? start "say")
                          (displayln (string-append "You say, \"" (concat remain) "\""))
                          (send (list 'chat username (concat remain))))
                         ((equal? start "examine")
                          (cond ((null? remain) (displayln "You think, \"What am I looking at?\""))
                                ((equal? (car remain) "tree")
                                 (displayln "You think, \"There is a lot of moss on this tree.\""))
                                ((equal? (car remain) "rocks")
                                 (displayln "You think, \"Most of these logs look pretty heavy.\""))
                                ((equal? (car remain) "log")
                                 (displayln "You think, \"It seems to have been sitting there for a long time.\""))
                                ((equal? (car remain) "people")
                                 (displayln "You think, \"They appear to be human.\""))))
                          
                         ((equal? input "look-around")
                          (displayln "You look around...")
                          (if (> (length (game-world 'get-users)) 1) (displayln "You see some trees, a log, some rocks, and other people.")
                              (displayln "You see some tree, a log, and some rocks.")))
                         (else (displayln (string-append "Error - no command found: " start)))))))
          
          (thread (lambda () 
          ; should be threaded: ?
          (define s-input (read in))
  
          ;(displayln s-input) ;tmp
  
          (cond ((not (eof-object? s-input))
                 (cond ((eq? (car s-input) 'disconnect)
                        (displayln (string-append (car (cdr s-input)) " has disconnected!")))
               
                       ((eq? (car s-input) 'connect)
                        (if (equal? (string->symbol (second s-input)) username) (displayln "You have connected to the server!")
                            (displayln (string-append (second s-input) " has connected!"))))
                       ((eq? (car s-input) 'chat)
                        (cond ((not (equal? (symbol->string (second s-input)) username))
                               (displayln (string-append (symbol->string (second s-input)) " says, \"" (third s-input) "\"")))))
                       ((eq? (car s-input) 'update-gamestate) ((game-world 'set-all-info) (second s-input)))
                       (else (display "Error ") (displayln s-input)))))))
                 
                 (client-input)))

; cleanup on exit
;(send (list 'disconnect username))
;(close-input-port in)
;(close-output-port out)