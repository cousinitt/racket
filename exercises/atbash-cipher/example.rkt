#lang racket

(provide (contract-out
          [encode (string? . -> . string?)]
          [decode (string? . -> . string?)]))

;; Private
(define (prepare-string text)
  (filter (λ (s) (or (char-numeric? s) (char-alphabetic? s)))
          (string->list (string-downcase text))))

(define atbash-char
  (let* ((a+ (curry + (char->integer #\a)))
         (z- (curry - (char->integer #\z)))
         (encipher (compose integer->char a+ z- char->integer)))
    (λ (char)
      (if (char-numeric? char) char (encipher char)))))

(define (windows size lst [acc '()])
  (if (>= (length lst) size)
      (let ((window (take lst size)))
        (windows size (drop lst size) (cons window acc)))
      (if (null? lst)
          (reverse acc)
          (reverse (cons lst acc)))))

(define translate (compose (curry map atbash-char) prepare-string))

;; Public
(define encode
  (let ((group (compose (curry map list->string) (curry windows 5))))
    (compose string-join group translate)))

(define decode (compose list->string translate ))
