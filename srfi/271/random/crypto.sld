;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
(define-library (srfi 271 random crypto)
  (export make-random-port)
  (import (scheme base)
          (scheme file))
  (begin
    (define (make-random-port . junk)
      (open-input-file "/dev/urandom"))
    ))
