#lang racket

; Here are some basic 
(define users '()) ; contains a list of lists, each represeting a user in the form (username (inputPort outputPort)) as seen below
(define (add-user user input output) (set! users (cons (list user input output) users)))

(define create-game-world ; is serializable due to serial-lambda
  (let ((users '()))
    (lambda (action)
      (cond ((eq? action 'get-users) users)
            
            ((eq? action 'add-user)
             (lambda (user input output)
               (set! users (cons (list user input output) users))))
            
            ((eq? action 'remove-user)
             (lambda (user)
               (set! users (filter (lambda (u) (not (equal? u user))) users))))

            ; get a copy of all local serializable data
            ((eq? action 'get-all-info)
             (define user-names '())
             (for-each (lambda (x) (set! user-names (cons (car x) user-names))) users)
             
             (list user-names)) ; to be updated regularly
            
            ((eq? action 'set-all-info)
             (lambda (info)
               (set! users (car info)))))
      )))
                                                      
(provide create-game-world)

