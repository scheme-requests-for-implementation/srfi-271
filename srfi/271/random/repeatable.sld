;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
(define-library (srfi 271 random repeatable)
  (export make-random-port
          random-port-state
          )
  (import (srfi 271 random repeatable xoshiro256++)))
