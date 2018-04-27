#lang racket

;; github.com/msafadieh

;; creates a vector with all letters of alphabet
(define alphabet
  (vector "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

;; finds a matching string in a vector
(define (find-in-vector x v)
  (letrec
      ((helper
        (lambda (c)
          (cond
            ;; returns false if the counter exceeds
            ;; the length of the vector
            [(>= c (vector-length v))
             #f]
            ;; if a match is found, return the counter
            [(string=? x (vector-ref v c))
             c]
            ;; keep trying otherwise
            [else
             (helper (add1 c))]))))
    ;; sets the counter to 0
    (helper 0)))

;; finds a matching string in a list
(define (find-in-list x listy)
  (let
      ((l (length listy)))
    (letrec
        ((helper
          (lambda (c listy)
            (cond
              ;; returns false if the counter exceeds
              ;; the length of the list
              [(>= c l)
               #f]
              ;; if a match is found, return the counter
              [(string=? (first listy) x)
               c]
              ;; otherwise keep trying
              [else (helper (add1 c) (rest listy))]))))
      ;; sets counter to 0
      (helper 0 listy))))

;; turns a string to a list of strings
(define (string->los s)
  ;; turns every char back into a string
  (map string
       ;; turns a string to a list of chars
       (string->list s)))

;; shifts a list n times
(define (shift n listy)
  (cond
    ;; if n = 0 just return the list
    [(zero? n)
     listy]
    ;; otherwise, append list without
    ;; the first element to a list containing
    ;; only that first element
    [else
      (shift (sub1 n)
             (append (rest listy)
                     (list (first listy))))]))


;; shifts the alphabet n times
(define (shifty n)
  (shift n (vector->list alphabet)))

;; finds the nth element of a list
(define (nth n listy)
  (cond
    ;; when n = 0 just return the first
    ;; element of the list
    [(= n 0)
     (first listy)]
    ;; otherwise, remove the first element
    ;; and subtract one from n
    [else
     (nth (sub1 n) (rest listy))]))

;; repeats a list until the length of the list is n
(define (repeat-n-times n listy)
  (let
      ((l (length listy))
       (k (- n (length listy))))
    (letrec ((helper
              (lambda (c listz)
                (cond
                  ;; if the counter is equal to the difference between
                  ;; the desired length and the actual length, return
                  ;; the list
                  [(= c k)
                   listz]
                  [else
                   ;; otherwise, add the correct nth element of
                   ;; the list to the end of that list
                   (helper (add1 c) (append listz
                                            (list (nth (modulo c l) listy))))]))))
  (helper 0 listy))))

;; encrypts a letter using a letter key
(define (encrypt-char c k)
  (cond
    ;; if the string is a space, return a space
    [(string=? c " ") " "]
    ;; otherwise, find the letter in the shifted alphabet
    ;; that exists at the same index as the input letter
    [else (nth (find-in-vector c alphabet) (shifty (find-in-vector k alphabet)))]))

;; encrypts every letter in a list of letters
(define (encryptor-list msg key)
  (map encrypt-char msg key))

;; encrypts a message with a key
(define (encryptor msg key)
  ;; appends all the elements in the encrypted list of strings
  (apply string-append
         (encryptor-list
          ;; turns the message into a list of strings
          (string->los msg)
          ;; turns the key into a list of strings and repeats
          ;; in enough times till it matches the length of msg
          (repeat-n-times (string-length msg) (string->los key)))))

;; decrypts a letter using a letter key
(define (decrypt-char c k)
  (cond
    ;; if the string is a space, return a space
    [(string=? c " ") " "]
    ;; otherwise, find the letter in the original alphabet
    ;; that exists at the same index in the shifted alphabet
    ;; as the input letter
    [else (vector-ref alphabet (find-in-list c (shifty (find-in-vector k alphabet))))]))

;; decrypts every letter in a list of letters
(define (decryptor-list msg key)
  (map decrypt-char msg key))

;; decrypts a message with a key
(define (decryptor msg key)
  ;; appends all the elements in the encrypted list of strings
  (apply string-append
         (decryptor-list
          ;; turns the message into a list of strings
          (string->los msg)
          ;; turns the key into a list of strings and repeats
          ;; in enough times till it matches the length of msg
          (repeat-n-times (string-length msg) (string->los key)))))