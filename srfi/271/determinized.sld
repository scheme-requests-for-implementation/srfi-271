;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT
(define-library (srfi 271 determinized)
  (export make-random-port
          random-port-state
          random-state=?
          random-port-initialization-error?
          )
  (import (srfi 271 determinized xoshiro256++)))
