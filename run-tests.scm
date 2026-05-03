;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
(import (scheme base)
        (scheme write)
        (srfi 64)
        (prefix (srfi NNN random crypto) crypto:)
        (prefix (srfi NNN random repeatable) repeat:)
        )

;;; Test runner

;; The SRFI 64 implementation used by most Schemes has a very basic
;; default test runner. This is slightly more helpful on failures.

(define (my-test-runner-factory)
  (let*
   ((runner (test-runner-null))
    (test-end
     (lambda (runner)
       (case (test-result-kind runner)
         ((pass)
          (display "Pass: ")
          (display (test-runner-test-name runner))
          (newline))
         ((fail)
          (display "FAIL: ")
          (display (test-runner-test-name runner))
          (display ". Expected ")
          (display (test-result-ref runner 'expected-value))
          (display ", got ")
          (display (test-result-ref runner 'actual-value))
          (display ".\n")))))
    (test-final
     (lambda (runner)
       (display "===============================\n")
       (display "Total passes: ")
       (display (test-runner-pass-count runner))
       (newline)
       (display "Total failures: ")
       (display (test-runner-fail-count runner))
       (newline)
       (display "Total skips: ")
       (display (test-runner-skip-count runner))
       (newline))))

    (test-runner-on-test-end! runner test-end)
    (test-runner-on-final! runner test-final)
    runner))

(test-runner-factory my-test-runner-factory)


(test-begin "Random ports")

(test-assert "crypto random ports are input ports"
  (input-port? (crypto:make-random-port)))

(test-assert "repeatable random ports are input ports"
  (input-port? (repeat:make-random-port)))

(test-assert "random-port-state returns a bytevector"
  (bytevector? (repeat:random-port-state (repeat:make-random-port))))

(test-assert "make-random-port (repeatable) accepts a bytevector"
  (let ((p1 (repeat:make-random-port)))
    (repeat:make-random-port (repeat:random-port-state p1))))

(test-assert "make-random-port (repeatable) accepts an input port"
  (let ((p1 (repeat:make-random-port)))
    (repeat:make-random-port p1)))

(test-end)
