;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
(define-library (srfi NNN random)
  (export make-random-port)
  (import (scheme base)
          (scheme file)
          (srfi NNN random crypto)))
