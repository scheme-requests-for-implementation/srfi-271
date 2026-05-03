;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
(define-library (srfi NNN random repeatable)
  (export make-random-port
          random-port-state
          )
  (import (srfi NNN random repeatable xoshiro256++)))
