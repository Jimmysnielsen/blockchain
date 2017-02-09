#lang racket
(require sha)
(require test-engine/racket-tests)

;; constants used for testing

(define BLOCK-ID0 0)
(define NONCE-0 0)
(define EMPTYSTR "")
(define BLOCK0 (string-append (number->string BLOCK-ID0) "/" (number->string NONCE-0) "/" EMPTYSTR "/")) 
(define SHA0 (sha256 (string->bytes/latin-1 BLOCK0)))
(define HASH0 (bytes->hex-string SHA0))
  
(define BLOCK-ID1 1)
(define NONCE-1 0)
(define STR-1 "Dette er en tekst jeg har skrevet")
(define BLOCK1 (string-append (number->string BLOCK-ID1) "/" (number->string NONCE-1) "/" STR-1 "/")) 
(define SHA1 (sha256 (string->bytes/latin-1 BLOCK1)))
(define HASH1 (bytes->hex-string SHA1))


;; functions

;; Natural Natural String -> Bytes
;; return a hash SHA256 value for a BLOCKCHAIN:
;;   Natural BlockID is [1,2,3...[ (unique, never reused)
;;   Natural Nonce is [0,1,2,...[
;;   String block-content is the content of the blockchain

(check-expect (my-hash BLOCK-ID0 NONCE-0 EMPTYSTR) SHA0)
(check-expect (my-hash BLOCK-ID1 NONCE-1 STR-1) SHA1)

;(define (my-hash blk-id nonce str) (sha256 #"")) ; stub
(define (my-hash blk-id nonce str)
  (sha256 (string->bytes/latin-1
           (string-append (number->string blk-id) "/" (number->string nonce) "/" str "/"))))

;; Bytes -> hex-string
;; return a hexadecimal representation of a SHA value
;; Bytes is SHA256
(check-expect (sha->hex SHA0) HASH0)
(check-expect (sha->hex SHA1) HASH1)

;(define (sha->hex sha) "/blk-id/nonce/text/") ;stub
(define (sha->hex sha) (bytes->hex-string sha))

;; Natural String -> (list Natural Hex-string)
;;   Block-id is [1,2,3,...[
;;   block-content is any String including ""
;; returns a '(NONCE and HASH) such that the SHA256 of "Block-id/NONCE/block-content/" starts with 4 times "0" (Zero)

(check-expect (mine-block 1 "") (list 2687 "00008fc16b8c2d3a91cdb2f0175741f73442d7ad20e1da335f8a7296360b5baf"))

;(define (mine-block blk-id str) (list 0 0)) ; stub

(define (mine-block blk-id str)
  (define (iter blk-id nonce str)
    (let* ([hash (sha->hex (my-hash blk-id nonce str))]
           [signature (substring hash 0 4)])
      (cond [(equal? signature "0000") (list nonce hash)]
            [(iter blk-id (+ 1 nonce) str)])))
  (iter blk-id 1 str))

;; now run all tests
(test)


















