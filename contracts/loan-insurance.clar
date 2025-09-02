;; title: Loan Insurance
;; version: 1.0.0
;; summary: Insurance policies to protect loan holders against borrower defaults
;; description: Enables loan NFT holders to purchase insurance coverage with risk-based premiums

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u300))
(define-constant err-not-found (err u301))
(define-constant err-unauthorized (err u302))
(define-constant err-invalid-amount (err u303))
(define-constant err-policy-expired (err u304))
(define-constant err-policy-active (err u305))
(define-constant err-claim-not-valid (err u306))
(define-constant err-already-claimed (err u307))
(define-constant err-insufficient-funds (err u308))
(define-constant err-policy-not-found (err u309))

(define-constant base-premium-rate u250) ;; 2.5% base rate
(define-constant max-coverage-percentage u80) ;; 80% max coverage
(define-constant policy-duration u8640) ;; ~60 days coverage
(define-constant claim-processing-fee u100) ;; 1% processing fee
(define-constant risk-multiplier-low u100) ;; 1x for low risk
(define-constant risk-multiplier-medium u150) ;; 1.5x for medium risk
(define-constant risk-multiplier-high u200) ;; 2x for high risk

;; data vars
(define-data-var next-policy-id uint u1)
(define-data-var total-premium-collected uint u0)
(define-data-var total-claims-paid uint u0)
(define-data-var insurance-pool uint u0)

;; data maps
(define-map insurance-policies
  uint
  {
    loan-id: uint,
    policy-holder: principal,
    coverage-amount: uint,
    premium-paid: uint,
    start-block: uint,
    end-block: uint,
    risk-level: uint,
    is-active: bool,
    claimed: bool
  }
)

(define-map loan-insurance-status
  uint
  {
    has-insurance: bool,
    policy-id: uint,
    coverage-percentage: uint,
    risk-assessment: uint
  }
)

(define-map insurance-claims
  uint
  {
    policy-id: uint,
    claimant: principal,
    claim-amount: uint,
    claim-block: uint,
    approved: bool,
    processed: bool,
    payout-amount: uint
  }
)

(define-map policyholder-stats
  principal
  {
    total-policies: uint,
    total-premiums-paid: uint,
    total-claims-received: uint,
    claim-success-rate: uint
  }
)

;; public functions
(define-public (purchase-insurance
  (loan-id uint)
  (coverage-percentage uint))
  (let ((policy-id (var-get next-policy-id))
        (current-block stacks-block-height)
        (risk-level (assess-loan-risk loan-id))
        (coverage-amount (calculate-coverage-amount loan-id coverage-percentage))
        (premium-amount (calculate-premium loan-id coverage-percentage risk-level)))
    
    (asserts! (<= coverage-percentage max-coverage-percentage) err-invalid-amount)
    (asserts! (> coverage-amount u0) err-invalid-amount)
    (asserts! (is-none (map-get? loan-insurance-status loan-id)) err-policy-active)
    
    ;; Transfer premium to insurance pool
    (try! (stx-transfer? premium-amount tx-sender (as-contract tx-sender)))
    
    (map-set insurance-policies policy-id {
      loan-id: loan-id,
      policy-holder: tx-sender,
      coverage-amount: coverage-amount,
      premium-paid: premium-amount,
      start-block: current-block,
      end-block: (+ current-block policy-duration),
      risk-level: risk-level,
      is-active: true,
      claimed: false
    })
    
    (map-set loan-insurance-status loan-id {
      has-insurance: true,
      policy-id: policy-id,
      coverage-percentage: coverage-percentage,
      risk-assessment: risk-level
    })
    
    (var-set total-premium-collected (+ (var-get total-premium-collected) premium-amount))
    (var-set insurance-pool (+ (var-get insurance-pool) premium-amount))
    (var-set next-policy-id (+ policy-id u1))
    
    (update-policyholder-stats tx-sender premium-amount)
    (ok policy-id)
  )
)

(define-public (file-insurance-claim (policy-id uint))
  (let ((policy (unwrap! (map-get? insurance-policies policy-id) err-policy-not-found))
        (current-block stacks-block-height))
    
    (asserts! (is-eq tx-sender (get policy-holder policy)) err-unauthorized)
    (asserts! (get is-active policy) err-policy-expired)
    (asserts! (< current-block (get end-block policy)) err-policy-expired)
    (asserts! (not (get claimed policy)) err-already-claimed)
    
    ;; Verify loan default status
    (asserts! (is-loan-defaulted (get loan-id policy)) err-claim-not-valid)
    
    (let ((claim-amount (get coverage-amount policy))
          (processing-fee (/ (* claim-amount claim-processing-fee) u10000))
          (payout-amount (- claim-amount processing-fee)))
      
      (asserts! (<= payout-amount (var-get insurance-pool)) err-insufficient-funds)
      
      ;; Process payout
      (try! (as-contract (stx-transfer? payout-amount tx-sender (get policy-holder policy))))
      
      (map-set insurance-policies policy-id (merge policy {
        claimed: true,
        is-active: false
      }))
      
      (map-set insurance-claims policy-id {
        policy-id: policy-id,
        claimant: tx-sender,
        claim-amount: claim-amount,
        claim-block: current-block,
        approved: true,
        processed: true,
        payout-amount: payout-amount
      })
      
      (var-set insurance-pool (- (var-get insurance-pool) payout-amount))
      (var-set total-claims-paid (+ (var-get total-claims-paid) payout-amount))
      
      (ok payout-amount)
    )
  )
)

(define-public (renew-policy (policy-id uint))
  (let ((policy (unwrap! (map-get? insurance-policies policy-id) err-policy-not-found))
        (current-block stacks-block-height)
        (new-premium (calculate-renewal-premium policy-id)))
    
    (asserts! (is-eq tx-sender (get policy-holder policy)) err-unauthorized)
    (asserts! (>= current-block (get end-block policy)) err-policy-active)
    (asserts! (not (get claimed policy)) err-already-claimed)
    
    ;; Pay renewal premium
    (try! (stx-transfer? new-premium tx-sender (as-contract tx-sender)))
    
    (map-set insurance-policies policy-id (merge policy {
      premium-paid: (+ (get premium-paid policy) new-premium),
      start-block: current-block,
      end-block: (+ current-block policy-duration),
      is-active: true
    }))
    
    (var-set total-premium-collected (+ (var-get total-premium-collected) new-premium))
    (var-set insurance-pool (+ (var-get insurance-pool) new-premium))
    
    (ok true)
  )
)

(define-public (contribute-to-pool (amount uint))
  (begin
    (asserts! (> amount u0) err-invalid-amount)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set insurance-pool (+ (var-get insurance-pool) amount))
    (ok (var-get insurance-pool))
  )
)

;; read-only functions
(define-read-only (get-insurance-policy (policy-id uint))
  (map-get? insurance-policies policy-id)
)

(define-read-only (get-loan-insurance-status (loan-id uint))
  (map-get? loan-insurance-status loan-id)
)

(define-read-only (get-insurance-quote (loan-id uint) (coverage-percentage uint))
  (let ((risk-level (assess-loan-risk loan-id))
        (coverage-amount (calculate-coverage-amount loan-id coverage-percentage))
        (premium (calculate-premium loan-id coverage-percentage risk-level)))
    
    (some {
      coverage-amount: coverage-amount,
      premium-required: premium,
      risk-level: risk-level,
      policy-duration-blocks: policy-duration
    })
  )
)

(define-read-only (get-insurance-stats)
  {
    total-policies: (var-get next-policy-id),
    total-premiums: (var-get total-premium-collected),
    total-claims: (var-get total-claims-paid),
    pool-balance: (var-get insurance-pool),
    pool-utilization: (if (> (var-get total-premium-collected) u0)
                        (/ (* (var-get total-claims-paid) u100) (var-get total-premium-collected))
                        u0)
  }
)

(define-read-only (get-policyholder-stats (holder principal))
  (map-get? policyholder-stats holder)
)

;; private functions
(define-private (assess-loan-risk (loan-id uint))
  ;; Simple risk assessment based on loan characteristics
  ;; Returns: 1 (low), 2 (medium), 3 (high)
  (let ((loan-details (contract-call? .Loantoken get-loan-details loan-id)))
    (match loan-details
      loan-data (let ((remaining-balance (get remaining-balance loan-data))
                      (original-amount (get original-amount loan-data))
                      (payments-made (get payments-made loan-data))
                      (loan-progress (if (> original-amount u0) 
                                      (/ (* (- original-amount remaining-balance) u100) original-amount) 
                                      u0)))
                  (if (and (> payments-made u6) (> loan-progress u50))
                    u1 ;; low risk
                    (if (and (> payments-made u3) (> loan-progress u25))
                      u2 ;; medium risk  
                      u3))) ;; high risk
      u3) ;; default to high risk if loan not found
  )
)

(define-private (calculate-coverage-amount (loan-id uint) (coverage-percentage uint))
  (let ((loan-details (contract-call? .Loantoken get-loan-details loan-id)))
    (match loan-details
      loan-data (/ (* (get remaining-balance loan-data) coverage-percentage) u100)
      u0)
  )
)

(define-private (calculate-premium (loan-id uint) (coverage-percentage uint) (risk-level uint))
  (let ((coverage-amount (calculate-coverage-amount loan-id coverage-percentage))
        (risk-multiplier (if (is-eq risk-level u1) 
                          risk-multiplier-low
                          (if (is-eq risk-level u2) 
                            risk-multiplier-medium 
                            risk-multiplier-high))))
    
    (/ (* (* coverage-amount base-premium-rate) risk-multiplier) u1000000)
  )
)

(define-private (calculate-renewal-premium (policy-id uint))
  (match (map-get? insurance-policies policy-id)
    policy (calculate-premium (get loan-id policy) 
                             (/ (* (get coverage-amount policy) u100) 
                                (calculate-coverage-amount (get loan-id policy) u100))
                             (get risk-level policy))
    u0)
)

(define-private (is-loan-defaulted (loan-id uint))
  ;; Check if loan is in default (simplified logic)
  (let ((loan-status (contract-call? .Loantoken get-loan-status loan-id)))
    (match loan-status
      ok-value (> (get months-behind ok-value) u2) ;; 2+ months behind = default
      err-value false)
  )
)

(define-private (update-policyholder-stats (holder principal) (premium-paid uint))
  (match (map-get? policyholder-stats holder)
    stats (map-set policyholder-stats holder (merge stats {
      total-policies: (+ (get total-policies stats) u1),
      total-premiums-paid: (+ (get total-premiums-paid stats) premium-paid)
    }))
    (map-set policyholder-stats holder {
      total-policies: u1,
      total-premiums-paid: premium-paid,
      total-claims-received: u0,
      claim-success-rate: u0
    })
  )
)
