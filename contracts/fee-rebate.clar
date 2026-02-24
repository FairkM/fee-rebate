;; fee-rebate.clar
;; ------------------------------------------------------------
;; STX Fee Rebate Contract
;;
;; - Protocol deposits collected fees
;; - Admin assigns rebate weight to users
;; - Users claim proportional rebates
;; - Gas-efficient cumulative accounting
;; ------------------------------------------------------------

(define-constant NOT_ADMIN u100)
(define-constant NO_WEIGHT u101)
(define-constant NO_REWARD u102)
(define-constant INVALID_AMOUNT u103)

(define-constant PRECISION u1000000)

;; -------------------------
;; Admin
;; -------------------------
(define-data-var admin principal tx-sender)

(define-public (set-admin (new-admin principal))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err NOT_ADMIN))
    (var-set admin new-admin)
    (ok true)
  )
)

;; -------------------------
;; Global Accounting
;; -------------------------

;; Total rebate weight
(define-data-var total-weight uint u0)

;; Accumulated rebate per weight (scaled 1e6)
(define-data-var acc-rebate-per-weight uint u0)

;; -------------------------
;; User Data
;; -------------------------

(define-map users
  { user: principal }
  {
    weight: uint,
    reward-debt: uint
  })

;; -------------------------
;; Events
;; -------------------------

(define-private (ev-fee-deposit (amount uint))
  (print { event: "fee-deposit", amount: amount }))

(define-private (ev-claim (user principal) (amount uint))
  (print { event: "rebate-claim", user: user, amount: amount }))

(define-private (ev-weight-update (user principal) (weight uint))
  (print { event: "weight-update", user: user, weight: weight }))

;; -------------------------
;; Admin Functions
;; -------------------------

(define-public (set-weight (user principal) (new-weight uint))
  (begin
    (asserts! (is-eq tx-sender (var-get admin)) (err NOT_ADMIN))

    (let ((current-weight (if (is-some (map-get? users { user: user }))
                              (get weight (unwrap-panic (map-get? users { user: user })))
                              u0)))
      (let ((new-debt (/ (* new-weight (var-get acc-rebate-per-weight)) PRECISION)))
        (begin
          (var-set total-weight
            (+ (- (var-get total-weight) current-weight) new-weight))
          (map-set users
            { user: user }
            {
              weight: new-weight,
              reward-debt: new-debt
            })
          (ev-weight-update user new-weight)
          (ok true)
        )
      )
    )
  )
)

;; -------------------------
;; Deposit Collected Fees
;; -------------------------

(define-public (deposit-fees (amount uint))
  (begin
    (asserts! (> amount u0) (err INVALID_AMOUNT))
    (asserts! (> (var-get total-weight) u0) (err NO_WEIGHT))

    ;; increase accumulated rebate per weight
    (var-set acc-rebate-per-weight
      (+ (var-get acc-rebate-per-weight)
         (/ (* amount PRECISION) (var-get total-weight))))

    (ev-fee-deposit amount)
    (ok true)
  )
)

;; -------------------------
;; Claim Rebate
;; -------------------------

(define-public (claim)
  (let ((user-data (map-get? users { user: tx-sender })))
    (asserts! (is-some user-data) (err NO_WEIGHT))

    (let (
          (data (unwrap-panic user-data))
          (weight (get weight data))
          (reward-debt (get reward-debt data))
          (accumulated (/ (* weight (var-get acc-rebate-per-weight)) PRECISION))
          (pending (- accumulated reward-debt))
         )

      (asserts! (> pending u0) (err NO_REWARD))

      ;; update reward debt
      (map-set users
        { user: tx-sender }
        {
          weight: weight,
          reward-debt: accumulated
        })

      ;; transfer STX rebate
      (asserts! (is-ok (stx-transfer? pending (as-contract tx-sender) tx-sender)) (err NO_REWARD))

      (ev-claim tx-sender pending)
      (ok pending)
    )
  )
)

;; -------------------------
;; Read-Only Functions
;; -------------------------

(define-read-only (get-user-info (user principal))
  (ok (map-get? users { user: user })))

(define-read-only (pending-rebate (user principal))
  (let ((user-data (map-get? users { user: user })))
    (if (is-none user-data)
        (ok u0)
        (let (
              (data (unwrap-panic user-data))
              (weight (get weight data))
              (reward-debt (get reward-debt data))
              (accumulated (/ (* weight (var-get acc-rebate-per-weight)) PRECISION))
             )
          (ok (- accumulated reward-debt))
        )
    )
  )
)

(define-read-only (get-total-weight)
  (ok (var-get total-weight)))

(define-read-only (get-admin)
  (ok (var-get admin)))