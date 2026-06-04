;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
(define-library (srfi 271 determinized xoshiro256++)
  (export make-random-port
          random-port-state
          random-state=?
          random-port-initialization-error?
          )
  (import (scheme base)
          (scheme bytevector)
          (scheme case-lambda)
          (scheme write)
          (srfi 151)
          (srfi 160 u64)
          (gauche base)
          (gauche keyword)
          (gauche vport)
          (prefix (srfi 271 randomized) r:)
          )
  (begin
    ;;; xoshiro256++ implementation transcribed from Wikipedia's
    ;;; C version (based on Vigna).

    ;;; WARNING: This is very much a sample implementation.  Please do
    ;;; not use this code in any application where security is a
    ;;; concern.  While I will try to fix any bugs uncovered in this
    ;;; implementation as they are revealed, I still request that you
    ;;; use something field-tested instead, preferably written and
    ;;; audited by competent numerical programmers.  My understanding
    ;;; of the arcana of pseudorandom number generation is minimal.

    (define state-number-of-bytes 32)

    (define mask (- (expt 2 64) 1))

    (define (+/mask a b)
      (bitwise-and (+ a b) mask))

    (define (ashift/mask n c)
      (bitwise-and (arithmetic-shift n c) mask))

    (define (rol64 x k)
      (bitwise-ior (ashift/mask x k)
                   (ashift/mask x (- k 64))))

    (define (xoshiro! state)
      (let ((s0 (u64vector-ref state 0))
            (s1 (u64vector-ref state 1))
            (s2 (u64vector-ref state 2))
            (s3 (u64vector-ref state 3)))
        (let ((result (+/mask (rol64 (+/mask s0 s3) 23) s0))
              (t (ashift/mask s1 17)))
          (u64vector-set! state 2 (bitwise-xor s2 s0))
          (u64vector-set! state 3 (bitwise-xor s3 s1))
          (u64vector-set! state 1 (bitwise-xor s1 s2))
          (u64vector-set! state 0 (bitwise-xor s0 s3))
          (u64vector-set! state 2 (bitwise-xor s2 t))
          (u64vector-set! state 3 (rol64 s3 45))
          result)))

    ;; Wrapper to get bytes out of the xoshiro generator.
    (define (xoshiro-bytes! state)
      (remainder (xoshiro! state) #x100))

    ;;; Init errors

    (define-condition-type &random-port-init
     &error
     random-port-initialization-error?)

    (define (random-port-initialization-error)
      (raise-continuable
       (condition
        (&message (message "not enough data to initialize port"))
        (&random-port-init))))

    ;;; xoshiro state manipulation

    (define (xoshiro-state? x)
      (and (u64vector? x)
           (= 4 (u64vector-length x))
           (not (u64vector-every zero? x))))

    (define BE (endianness big))

    (define (make-state-from-port port)
      (let ((bvec (read-bytevector state-number-of-bytes port)))
        (when (eof-object? bvec)
          (random-port-initialization-error))
        (u64vector (bytevector-u64-ref bvec 0 BE)
                   (bytevector-u64-ref bvec 8 BE)
                   (bytevector-u64-ref bvec 16 BE)
                   (bytevector-u64-ref bvec 24 BE))))

    ;; Based on Vigna's public-domain splitmix64.
    (define (make-state-from-integer x)
      (let ((next
             (lambda ()
               (set! x (+/mask x #x9e3779b97f4a7c15))
               (let ((z (*/mask (bitwise-xor x (ashift/mask x -30))
                                #xbf58476d1ce4e5b9)))
                 (set! z (*/mask (bitwise-xor z (ashift/mask z -27))
                                 #x94d049bb133111eb))
                 (bitwise-xor z (ashift/mask z -31)))))
            (state (make-u64vector 4)))
        (u64vector-set! state 0 (next))
        (u64vector-set! state 1 (next))
        (u64vector-set! state 2 (next))
        (u64vector-set! state 3 (next))
        state))

    ;; Exported
    (define (random-state=? state1 state2 . rest-states)
      (let ((check-state
             (lambda (x)
               (unless (xoshiro-state? x)
                 (error "invalid argument: not a random-port state"
                        x)))))
        (check-state state1)
        (check-state state2)
        (for-each check-state rest-states)
        (apply u64vector= state1 state2 rest-states)))

    ;;; xoshiro warmup

    ;; Returns the total number of 1 bits in *state*'s elements.
    (define (xoshiro-state-bit-count state)
      (u64vector-fold (lambda (s k) (+ s (bit-count k)))
                      0
                      state))

    ;; True if the binary representation of *state* has an
    ;; approximately even distribution of 1s and 0s.
    (define (xoshiro-state-scrambled? state)
      (let ((ratio (/ (xoshiro-state-bit-count state)
                      (* state-number-of-bytes 8))))
        (< 0.48 ratio 0.52)))

    ;; Run at least this many warmup cycles.
    (define minimum-warmup-cycles 8)

    ;; Give up and signal an initialization error if a scrambled
    ;; xoshiro state can't be obtained after this number of warmup
    ;; cycles.
    (define maximum-warmup-cycles 1024)

    (define (random-port-warmup! port)
      (letrec*
       ((c 0)
        (warmup!
         (lambda ()
           (cond ((and (>= c minimum-warmup-cycles)
                       (xoshiro-state-scrambled?
                        (random-port-state port))))
                 ((>= c maximum-warmup-cycles)
                  (random-port-initialization-error))
                 (else
                  (set! c (+ c 1))
                  (read-u8 port)
                  (warmup!))))))
        (warmup!)
        port))

    (define (make-xoshiro-random-port init)
      (make <random-port> init xoshiro-bytes!))

    (define make-random-port
      (case-lambda
        (()
         (call-with-port (r:make-random-port) make-random-port))
        ((initializer)
         (let* ((init
                 (cond ((input-port? initializer)
                        (make-state-from-port initializer))
                       ((xoshiro-state? initializer)
                        (u64vector-copy initializer))
                       (else
                        (error "make-random-port: invalid initializer"
                               initializer))))
                (port (make-xoshiro-random-port init)))
           (unless (xoshiro-state? initializer)
             (random-port-warmup! port))
           port))))

    ;;; Gauche virtual port type

    ;; The Gauche object system is new to me. This probably needs
    ;; work.
    ;;
    ;; Here's how it works so far:
    ;;
    ;; A gen-and-transform! procedure is passed to the <random-port>
    ;; initializer even though it does not correspond to a field.
    ;; It is expected to both mutate the state slot and return a byte
    ;; when invoked.  If 'getb' were a virtual-port method rather
    ;; than an instance variable, we could do things differently,
    ;; since a method would have access to the port object itself.

    (define-class <random-port> (<virtual-input-port>)
      ((state :getter random-port-state)))

    (define-method initialize ((self <random-port>) initargs)
      (let ((state (car initargs))
            (gen-&-transform! (cadr initargs)))
        (next-method)
        (slot-set! self 'state state)
        (slot-set! self 'getb (lambda () (gen-&-transform! state)))))
    ))
