;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
(define-library (srfi 271 determinized xoshiro256++)
  (export make-random-port
          random-port-state
          random-port-initialization-error?
          )
  (import (scheme base)
          ;; The native ref forms are buggy in Gauche 0.9.15.
          (except (scheme bytevector) bytevector-u64-native-ref)
          (scheme case-lambda)
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

    ;; TODO: Remove me when Gauche's version is fixed.
    (define (bytevector-u64-native-ref bvec k)
      (bytevector-u64-ref bvec k (native-endianness)))

    (define (make-state-from-bytevector bvec)
      (when (< (bytevector-length bvec) state-number-of-bytes)
        (random-port-initialization-error))
      (u64vector (bytevector-u64-native-ref bvec 0)
                 (bytevector-u64-native-ref bvec 8)
                 (bytevector-u64-native-ref bvec 16)
                 (bytevector-u64-native-ref bvec 24)))

    (define (make-state-from-port port)
      (let ((bvec (read-bytevector state-number-of-bytes port)))
        (when (eof-object? bvec)
          (random-port-initialization-error))
        (make-state-from-bytevector bvec)))

    (define (make-xoshiro-random-port init)
      (make <random-port> init xoshiro-bytes!))

    (define make-random-port
      (case-lambda
        (()
         (call-with-port (r:make-random-port) make-random-port))
        ((initializer)
         (let ((init
                (cond ((input-port? initializer)
                       (make-state-from-port initializer))
                      ((bytevector? initializer)
                       (make-state-from-bytevector initializer))
                      (else
                       (error "make-random-port: invalid initializer"
                              initializer)))))
           (make-xoshiro-random-port init)))))

    ;; Get the state from *xport* and convert it to a bytevector.
    (define (random-port-state xport)
      (let ((state (random-port-raw-state xport))
            (bvec (make-bytevector state-number-of-bytes)))
        (do ((i 0 (+ i 1))
             (k 0 (+ k 8)))
            ((= i 4) bvec)
          (bytevector-u64-native-set! bvec
                                      k
                                      (u64vector-ref state i)))))

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
      ((state :getter random-port-raw-state)))

    (define-method initialize ((self <random-port>) initargs)
      (let ((state (car initargs))
            (gen-&-transform! (cadr initargs)))
        (next-method)
        (slot-set! self 'state state)
        (slot-set! self 'getb (lambda () (gen-&-transform! state)))))
    ))
