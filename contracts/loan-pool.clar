;; -------------------------------------------------------------
;; Contract: loan-pool.clar
;; Description:
;; A basic STX lending pool with collateralized borrowing.
;; Users deposit STX as collateral and borrow from the pooled liquidity.
;; Collateral ratio enforces safe borrowing.
;; -------------------------------------------------------------

(define-constant ERR-NOT-OWNER u100)
(define-constant ERR-ZERO u101)
(define-constant ERR-NO-COLLATERAL u102)
(define-constant ERR-INSUFFICIENT-COLLATERAL u103)
(define-constant ERR-INSUFFICIENT-LIQUIDITY u104)
(define-constant ERR-NO-DEBT u105)
(define-constant ERR-ALREADY-INITIALIZED u106)
(define-constant ERR-UNCHECKED-DATA u107)

;; -------------------------
;; State
;; -------------------------
(define-data-var owner (optional principal) none)
(define-data-var loan-limit-rate uint u150) ;; 150% collateralization

(define-data-var total-liquidity uint u0) ;; Pool funds

;; Per-user vault
(define-map user-vaults
  { user: principal }
  { collateral: uint, debt: uint }
)

;; -------------------------
;; Init Ownership
;; -------------------------
(define-public (initialize)
  (match (var-get owner)
    some-owner (err ERR-ALREADY-INITIALIZED)
    (begin
      (var-set owner (some tx-sender))
      (ok tx-sender)
    )
  )
)

;; -------------------------
;; Internal helper
;; -------------------------
(define-private (collat-sufficient? (coll uint) (debt uint))
  (if (<= debt u0)
      true
      (>= (* coll u100) (* debt (var-get loan-limit-rate)))
  )
)

;; -------------------------
;; Collateral Deposit
;; Users send STX to increase collateral
;; -------------------------
(define-public (deposit-collateral (amt uint))
  (if (<= amt u0)
      (err ERR-ZERO)
      (let ((sender tx-sender)
            (vault (default-to { collateral: u0, debt: u0 }
                               (map-get? user-vaults { user: sender }))))
        (begin
          (map-set user-vaults
                   { user: sender }
                   { collateral: (+ (get collateral vault) amt),
                     debt: (get debt vault)
                   })
          (ok (+ (get collateral vault) amt))
        )
      )
  )
)

;; -------------------------
;; Borrow from pool
;; -------------------------
(define-public (borrow (amt uint))
  (if (<= amt u0)
      (err ERR-ZERO)
      (let ((sender tx-sender)
            (pool (var-get total-liquidity)))
        (if (< pool amt)
            (err ERR-INSUFFICIENT-LIQUIDITY)
            (match (map-get? user-vaults { user: sender })
              some-vault
                (let ((coll (get collateral some-vault))
                      (new-debt (+ (get debt some-vault) amt)))
                  (if (not (collat-sufficient? coll new-debt))
                      (err ERR-INSUFFICIENT-COLLATERAL)
                      (begin
                        (var-set total-liquidity (- pool amt))
                        (map-set user-vaults
                                 { user: sender }
                                 { collateral: coll, debt: new-debt })
                        (ok true)
                      )
                  )
                )
              (err ERR-NO-COLLATERAL)
            )
        )
      )
  )
)

;; -------------------------
;; Repay a loan
;; Caller sends STX with this call
;; -------------------------
(define-public (repay (amt uint))
  (if (<= amt u0)
      (err ERR-ZERO)
      (let ((sender tx-sender))
        (match (map-get? user-vaults { user: sender })
          some-v
            (let ((debt (get debt some-v)))
              (if (<= debt u0)
                  (err ERR-NO-DEBT)
                  (let ((repay-amt (if (> amt debt) debt amt)))
                    (begin
                      (var-set total-liquidity (+ (var-get total-liquidity) repay-amt))
                      (map-set user-vaults
                               { user: sender }
                               { collateral: (get collateral some-v),
                                 debt: (- debt repay-amt)
                               })
                      (ok true)
                    )
                  )
              )
            )
          (err ERR-NO-DEBT)
        )
      )
  )
)

;; -------------------------
;; Withdraw collateral (only if no debt remains)
;; -------------------------
(define-public (withdraw-collateral (amt uint))
  (if (<= amt u0)
      (err ERR-ZERO)
      (let ((sender tx-sender))
        (match (map-get? user-vaults { user: sender })
          some-v
            (let ((coll (get collateral some-v)))
              (if (> (get debt some-v) u0)
                  (err ERR-NO-DEBT)
                  (if (< coll amt)
                      (err ERR-INSUFFICIENT-COLLATERAL)
                      (begin
                        (map-set user-vaults
                                 { user: sender }
                                 { collateral: (- coll amt), debt: u0 })
                        (ok true)
                      )
                  )
              )
            )
          (err ERR-NO-COLLATERAL)
        )
      )
  )
)

;; -------------------------
;; Liquidity Supply
;; Lenders deposit STX to increase pool
;; -------------------------
(define-public (supply-liquidity (amt uint))
  (if (<= amt u0)
      (err ERR-ZERO)
      (begin
        (var-set total-liquidity (+ (var-get total-liquidity) amt))
        (ok (var-get total-liquidity))
      )
  )
)

;; Owner-only: withdraw liquidity (not recommended if loans exist)
(define-public (owner-withdraw (amt uint))
  (match (var-get owner)
    some-owner
      (if (not (is-eq tx-sender some-owner))
          (err ERR-NOT-OWNER)
          (let ((pool (var-get total-liquidity)))
            (if (> amt pool)
                (err ERR-INSUFFICIENT-LIQUIDITY)
                (begin
                  (var-set total-liquidity (- pool amt))
                  (ok true)
                )
            )
          )
      )
    (err ERR-NOT-OWNER)
  )
)

;; -------------------------
;; Read-only helpers
;; -------------------------
(define-read-only (get-vault (user principal))
  (default-to { collateral: u0, debt: u0 }
              (map-get? user-vaults { user: user }))
)

(define-read-only (get-total-liquidity)
  (var-get total-liquidity)
)

(define-read-only (get-loan-limit-rate)
  (var-get loan-limit-rate)
)
