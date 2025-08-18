
(define-non-fungible-token student-loan uint)

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))
(define-constant err-listing-not-found (err u102))
(define-constant err-not-for-sale (err u103))
(define-constant err-insufficient-payment (err u104))
(define-constant err-loan-not-found (err u105))
(define-constant err-invalid-amount (err u106))
(define-constant err-loan-already-paid (err u107))
(define-constant err-payment-failed (err u108))
(define-constant err-insufficient-collateral (err u109))
(define-constant err-collateral-locked (err u110))
(define-constant err-liquidation-not-allowed (err u111))
(define-constant err-collateral-not-found (err u112))
(define-constant err-invalid-collateral-ratio (err u113))
(define-constant err-loan-not-defaulted (err u114))
(define-constant err-not-authorized-servicer (err u115))
(define-constant err-servicer-already-assigned (err u116))
(define-constant err-invalid-servicer-fee (err u117))
(define-constant err-servicer-not-found (err u118))
(define-constant err-servicer-suspended (err u119))

(define-data-var last-token-id uint u0)
(define-data-var total-loans-issued uint u0)
(define-data-var total-amount-loaned uint u0)

(define-map loan-details
  uint
  {
    borrower: principal,
    original-amount: uint,
    remaining-balance: uint,
    interest-rate: uint,
    term-months: uint,
    issue-block: uint,
    monthly-payment: uint,
    payments-made: uint,
    is-active: bool
  }
)

(define-map loan-listings
  uint
  {
    seller: principal,
    price: uint,
    is-active: bool
  }
)

(define-map borrower-loans principal (list 50 uint))
(define-map payment-history uint (list 100 {block: uint, amount: uint, remaining: uint}))

(define-map loan-collateral
  uint
  {
    stx-amount: uint,
    collateral-ratio: uint,
    min-ratio: uint,
    is-locked: bool,
    deposited-block: uint
  }
)

(define-map collateral-liquidations
  uint
  {
    liquidator: principal,
    liquidation-block: uint,
    collateral-seized: uint,
    debt-recovered: uint
  }
)

(define-data-var liquidation-penalty uint u10)
(define-data-var min-collateral-ratio uint u150)

(define-map loan-servicers
  uint
  {
    servicer: principal,
    fee-percentage: uint,
    assigned-block: uint,
    is-active: bool,
    payments-processed: uint,
    total-fees-earned: uint
  }
)

(define-map servicer-profiles
  principal
  {
    is-registered: bool,
    is-suspended: bool,
    total-loans-serviced: uint,
    total-payments-processed: uint,
    reputation-score: uint,
    registration-block: uint
  }
)

(define-map servicer-disputes
  {servicer: principal, loan-id: uint}
  {
    dispute-reason: uint,
    filed-block: uint,
    is-resolved: bool,
    resolution-block: uint
  }
)

(define-data-var max-servicer-fee uint u500)

(define-public (get-last-token-id)
  (ok (var-get last-token-id))
)

(define-public (get-token-uri (token-id uint))
  (ok none)
)

(define-public (get-owner (token-id uint))
  (ok (nft-get-owner? student-loan token-id))
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) err-not-token-owner)
    (asserts! (is-eq sender (unwrap! (nft-get-owner? student-loan token-id) err-not-token-owner)) err-not-token-owner)
    (nft-transfer? student-loan token-id sender recipient)
  )
)

(define-public (issue-loan 
  (borrower principal) 
  (amount uint) 
  (interest-rate uint) 
  (term-months uint))
  (let
    (
      (token-id (+ (var-get last-token-id) u1))
    ;;   (monthly-payment (calculate-monthly-payment amount interest-rate term-months))
      (current-loans (default-to (list) (map-get? borrower-loans borrower)))
    )
    (asserts! (> amount u0) err-invalid-amount)
    (asserts! (> term-months u0) err-invalid-amount)
    (asserts! (> interest-rate u0) err-invalid-amount)
    
    (try! (nft-mint? student-loan token-id contract-owner))
    
    (map-set loan-details token-id
      {
        borrower: borrower,
        original-amount: amount,
        remaining-balance: amount,
        interest-rate: interest-rate,
        term-months: term-months,
        issue-block: stacks-block-height,
        monthly-payment: u1,
        payments-made: u0,
        is-active: true
      }
    )
    
    (map-set borrower-loans borrower (unwrap! (as-max-len? (append current-loans token-id) u50) err-invalid-amount))
    
    (var-set last-token-id token-id)
    (var-set total-loans-issued (+ (var-get total-loans-issued) u1))
    (var-set total-amount-loaned (+ (var-get total-amount-loaned) amount))
    
    (ok token-id)
  )
)

(define-public (make-payment (token-id uint) (payment-amount uint))
  (let
    (
      (loan (unwrap! (map-get? loan-details token-id) err-loan-not-found))
      (current-balance (get remaining-balance loan))
      (payment-history-list (default-to (list) (map-get? payment-history token-id)))
      (new-balance (if (>= payment-amount current-balance) u0 (- current-balance payment-amount)))
      (actual-payment (if (>= payment-amount current-balance) current-balance payment-amount))
    )
    (asserts! (get is-active loan) err-loan-already-paid)
    (asserts! (> payment-amount u0) err-invalid-amount)
    (asserts! (> current-balance u0) err-loan-already-paid)
    (asserts! (is-eq tx-sender (get borrower loan)) err-not-token-owner)
    
    (try! (stx-transfer? actual-payment tx-sender (unwrap! (nft-get-owner? student-loan token-id) err-not-token-owner)))
    
    (map-set loan-details token-id
      (merge loan
        {
          remaining-balance: new-balance,
          payments-made: (+ (get payments-made loan) u1),
          is-active: (> new-balance u0)
        }
      )
    )
    
    (map-set payment-history token-id
      (unwrap! (as-max-len? 
        (append payment-history-list 
          {block: stacks-block-height, amount: actual-payment, remaining: new-balance}) 
        u100) 
        err-payment-failed)
    )
    
    (ok new-balance)
  )
)

(define-public (list-loan-for-sale (token-id uint) (price uint))
  (let
    (
      (owner (unwrap! (nft-get-owner? student-loan token-id) err-not-token-owner))
    )
    (asserts! (is-eq tx-sender owner) err-not-token-owner)
    (asserts! (> price u0) err-invalid-amount)
    
    (map-set loan-listings token-id
      {
        seller: tx-sender,
        price: price,
        is-active: true
      }
    )
    
    (ok true)
  )
)

(define-public (buy-loan (token-id uint))
  (let
    (
      (listing (unwrap! (map-get? loan-listings token-id) err-listing-not-found))
      (seller (get seller listing))
      (price (get price listing))
    )
    (asserts! (get is-active listing) err-not-for-sale)
    (asserts! (not (is-eq tx-sender seller)) err-not-token-owner)
    
    (try! (stx-transfer? price tx-sender seller))
    (try! (nft-transfer? student-loan token-id seller tx-sender))
    
    (map-set loan-listings token-id
      (merge listing {is-active: false})
    )
    
    (ok true)
  )
)

(define-public (cancel-listing (token-id uint))
  (let
    (
      (listing (unwrap! (map-get? loan-listings token-id) err-listing-not-found))
      (owner (unwrap! (nft-get-owner? student-loan token-id) err-not-token-owner))
    )
    (asserts! (is-eq tx-sender owner) err-not-token-owner)
    (asserts! (get is-active listing) err-not-for-sale)
    
    (map-set loan-listings token-id
      (merge listing {is-active: false})
    )
    
    (ok true)
  )
)

(define-read-only (get-loan-details (token-id uint))
  (map-get? loan-details token-id)
)

(define-read-only (get-loan-listing (token-id uint))
  (map-get? loan-listings token-id)
)

(define-read-only (get-borrower-loans (borrower principal))
  (map-get? borrower-loans borrower)
)

(define-read-only (get-payment-history (token-id uint))
  (map-get? payment-history token-id)
)

(define-read-only (get-loan-status (token-id uint))
  (match (map-get? loan-details token-id)
    loan-data
      (let
        (
          (blocks-since-issue (- stacks-block-height (get issue-block loan-data)))
          (months-elapsed (/ blocks-since-issue u144))
          (expected-payments months-elapsed)
          (actual-payments (get payments-made loan-data))
        )
        (ok {
          is-current: (>= actual-payments expected-payments),
          months-behind: (if (> expected-payments actual-payments) (- expected-payments actual-payments) u0),
          completion-percentage: (if (> (get original-amount loan-data) u0) 
            (/ (* (- (get original-amount loan-data) (get remaining-balance loan-data)) u100) (get original-amount loan-data)) 
            u0)
        })
      )
    err-loan-not-found
  )
)

(define-read-only (get-contract-stats)
  (ok {
    total-loans: (var-get total-loans-issued),
    total-amount: (var-get total-amount-loaned),
    last-token-id: (var-get last-token-id)
  })
)

(define-public (deposit-collateral (token-id uint) (stx-amount uint))
  (let
    (
      (loan (unwrap! (map-get? loan-details token-id) err-loan-not-found))
      (existing-collateral (default-to {stx-amount: u0, collateral-ratio: u0, min-ratio: (var-get min-collateral-ratio), is-locked: false, deposited-block: u0} (map-get? loan-collateral token-id)))
      (loan-amount (get remaining-balance loan))
      (total-collateral (+ (get stx-amount existing-collateral) stx-amount))
      (new-ratio (if (> loan-amount u0) (/ (* total-collateral u100) loan-amount) u0))
    )
    (asserts! (is-eq tx-sender (get borrower loan)) err-not-token-owner)
    (asserts! (get is-active loan) err-loan-already-paid)
    (asserts! (> stx-amount u0) err-invalid-amount)
    
    (try! (stx-transfer? stx-amount tx-sender (as-contract tx-sender)))
    
    (map-set loan-collateral token-id
      {
        stx-amount: total-collateral,
        collateral-ratio: new-ratio,
        min-ratio: (get min-ratio existing-collateral),
        is-locked: true,
        deposited-block: stacks-block-height
      }
    )
    
    (ok total-collateral)
  )
)

(define-public (withdraw-collateral (token-id uint) (withdraw-amount uint))
  (let
    (
      (loan (unwrap! (map-get? loan-details token-id) err-loan-not-found))
      (collateral (unwrap! (map-get? loan-collateral token-id) err-collateral-not-found))
      (loan-amount (get remaining-balance loan))
      (current-collateral (get stx-amount collateral))
      (remaining-collateral (if (>= withdraw-amount current-collateral) u0 (- current-collateral withdraw-amount)))
      (actual-withdraw (if (>= withdraw-amount current-collateral) current-collateral withdraw-amount))
      (new-ratio (if (and (> loan-amount u0) (> remaining-collateral u0)) (/ (* remaining-collateral u100) loan-amount) u0))
    )
    (asserts! (is-eq tx-sender (get borrower loan)) err-not-token-owner)
    (asserts! (> withdraw-amount u0) err-invalid-amount)
    (asserts! (get is-locked collateral) err-collateral-not-found)
    (asserts! (or (is-eq loan-amount u0) (>= new-ratio (get min-ratio collateral))) err-insufficient-collateral)
    
    (try! (as-contract (stx-transfer? actual-withdraw tx-sender (get borrower loan))))
    
    (map-set loan-collateral token-id
      (merge collateral
        {
          stx-amount: remaining-collateral,
          collateral-ratio: new-ratio,
          is-locked: (> remaining-collateral u0)
        }
      )
    )
    
    (ok remaining-collateral)
  )
)

(define-public (liquidate-loan (token-id uint))
  (let
    (
      (loan (unwrap! (map-get? loan-details token-id) err-loan-not-found))
      (collateral (unwrap! (map-get? loan-collateral token-id) err-collateral-not-found))
      (loan-owner (unwrap! (nft-get-owner? student-loan token-id) err-not-token-owner))
      (remaining-balance (get remaining-balance loan))
      (collateral-amount (get stx-amount collateral))
      (current-ratio (get collateral-ratio collateral))
      (penalty-amount (/ (* collateral-amount (var-get liquidation-penalty)) u100))
      (liquidation-amount (- collateral-amount penalty-amount))
      (debt-coverage (if (>= liquidation-amount remaining-balance) remaining-balance liquidation-amount))
      (excess-collateral (if (> liquidation-amount remaining-balance) (- liquidation-amount remaining-balance) u0))
    )
    (asserts! (get is-active loan) err-loan-already-paid)
    (asserts! (get is-locked collateral) err-collateral-not-found)
    (asserts! (< current-ratio (get min-ratio collateral)) err-liquidation-not-allowed)
    (asserts! (> remaining-balance u0) err-loan-already-paid)
    
    (try! (as-contract (stx-transfer? debt-coverage tx-sender loan-owner)))
    
    (if (> excess-collateral u0)
      (try! (as-contract (stx-transfer? excess-collateral tx-sender (get borrower loan))))
      true
    )
    
    (map-set loan-details token-id
      (merge loan
        {
          remaining-balance: (if (>= debt-coverage remaining-balance) u0 (- remaining-balance debt-coverage)),
          is-active: (< debt-coverage remaining-balance)
        }
      )
    )
    
    (map-set loan-collateral token-id
      (merge collateral
        {
          stx-amount: u0,
          collateral-ratio: u0,
          is-locked: false
        }
      )
    )
    
    (map-set collateral-liquidations token-id
      {
        liquidator: tx-sender,
        liquidation-block: stacks-block-height,
        collateral-seized: collateral-amount,
        debt-recovered: debt-coverage
      }
    )
    
    (ok debt-coverage)
  )
)

(define-public (set-collateral-requirements (token-id uint) (min-ratio uint))
  (let
    (
      (loan (unwrap! (map-get? loan-details token-id) err-loan-not-found))
      (collateral (default-to {stx-amount: u0, collateral-ratio: u0, min-ratio: (var-get min-collateral-ratio), is-locked: false, deposited-block: u0} (map-get? loan-collateral token-id)))
      (loan-owner (unwrap! (nft-get-owner? student-loan token-id) err-not-token-owner))
    )
    (asserts! (is-eq tx-sender loan-owner) err-not-token-owner)
    (asserts! (>= min-ratio u100) err-invalid-collateral-ratio)
    (asserts! (<= min-ratio u500) err-invalid-collateral-ratio)
    
    (map-set loan-collateral token-id
      (merge collateral {min-ratio: min-ratio})
    )
    
    (ok min-ratio)
  )
)

(define-public (update-liquidation-settings (penalty uint) (min-ratio uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= penalty u50) err-invalid-amount)
    (asserts! (>= min-ratio u100) err-invalid-collateral-ratio)
    (asserts! (<= min-ratio u500) err-invalid-collateral-ratio)
    
    (var-set liquidation-penalty penalty)
    (var-set min-collateral-ratio min-ratio)
    
    (ok true)
  )
)

(define-read-only (get-collateral-details (token-id uint))
  (map-get? loan-collateral token-id)
)

(define-read-only (get-liquidation-details (token-id uint))
  (map-get? collateral-liquidations token-id)
)

(define-read-only (calculate-collateral-ratio (token-id uint))
  (match (map-get? loan-details token-id)
    loan-data
      (match (map-get? loan-collateral token-id)
        collateral-data
          (let
            (
              (remaining-balance (get remaining-balance loan-data))
              (collateral-amount (get stx-amount collateral-data))
            )
            (ok (if (> remaining-balance u0) (/ (* collateral-amount u100) remaining-balance) u0))
          )
        (ok u0)
      )
    err-loan-not-found
  )
)

(define-read-only (is-loan-eligible-for-liquidation (token-id uint))
  (match (map-get? loan-collateral token-id)
    collateral-data
      (let
        (
          (current-ratio (get collateral-ratio collateral-data))
          (min-ratio (get min-ratio collateral-data))
          (is-locked (get is-locked collateral-data))
        )
        (ok (and is-locked (< current-ratio min-ratio)))
      )
    (ok false)
  )
)

(define-read-only (get-liquidation-settings)
  (ok {
    penalty: (var-get liquidation-penalty),
    min-ratio: (var-get min-collateral-ratio)
  })
)

(define-public (register-servicer)
  (let
    (
      (existing-profile (default-to {is-registered: false, is-suspended: false, total-loans-serviced: u0, total-payments-processed: u0, reputation-score: u100, registration-block: u0} (map-get? servicer-profiles tx-sender)))
    )
    (asserts! (not (get is-registered existing-profile)) err-servicer-already-assigned)
    
    (map-set servicer-profiles tx-sender
      {
        is-registered: true,
        is-suspended: false,
        total-loans-serviced: u0,
        total-payments-processed: u0,
        reputation-score: u100,
        registration-block: stacks-block-height
      }
    )
    
    (ok true)
  )
)

(define-public (assign-servicer (token-id uint) (servicer principal) (fee-percentage uint))
  (let
    (
      (loan (unwrap! (map-get? loan-details token-id) err-loan-not-found))
      (loan-owner (unwrap! (nft-get-owner? student-loan token-id) err-not-token-owner))
      (servicer-profile (unwrap! (map-get? servicer-profiles servicer) err-servicer-not-found))
      (existing-assignment (map-get? loan-servicers token-id))
    )
    (asserts! (is-eq tx-sender loan-owner) err-not-token-owner)
    (asserts! (get is-registered servicer-profile) err-servicer-not-found)
    (asserts! (not (get is-suspended servicer-profile)) err-servicer-suspended)
    (asserts! (<= fee-percentage (var-get max-servicer-fee)) err-invalid-servicer-fee)
    (asserts! (is-none existing-assignment) err-servicer-already-assigned)
    (asserts! (get is-active loan) err-loan-already-paid)
    
    (map-set loan-servicers token-id
      {
        servicer: servicer,
        fee-percentage: fee-percentage,
        assigned-block: stacks-block-height,
        is-active: true,
        payments-processed: u0,
        total-fees-earned: u0
      }
    )
    
    (map-set servicer-profiles servicer
      (merge servicer-profile
        {
          total-loans-serviced: (+ (get total-loans-serviced servicer-profile) u1)
        }
      )
    )
    
    (ok true)
  )
)

(define-public (revoke-servicer (token-id uint))
  (let
    (
      (loan-owner (unwrap! (nft-get-owner? student-loan token-id) err-not-token-owner))
      (servicer-assignment (unwrap! (map-get? loan-servicers token-id) err-servicer-not-found))
    )
    (asserts! (is-eq tx-sender loan-owner) err-not-token-owner)
    (asserts! (get is-active servicer-assignment) err-servicer-not-found)
    
    (map-set loan-servicers token-id
      (merge servicer-assignment {is-active: false})
    )
    
    (ok true)
  )
)

(define-public (servicer-collect-payment (token-id uint) (payment-amount uint))
  (let
    (
      (loan (unwrap! (map-get? loan-details token-id) err-loan-not-found))
      (servicer-assignment (unwrap! (map-get? loan-servicers token-id) err-servicer-not-found))
      (servicer-profile (unwrap! (map-get? servicer-profiles tx-sender) err-servicer-not-found))
      (loan-owner (unwrap! (nft-get-owner? student-loan token-id) err-not-token-owner))
      (current-balance (get remaining-balance loan))
      (payment-history-list (default-to (list) (map-get? payment-history token-id)))
      (new-balance (if (>= payment-amount current-balance) u0 (- current-balance payment-amount)))
      (actual-payment (if (>= payment-amount current-balance) current-balance payment-amount))
      (servicer-fee (/ (* actual-payment (get fee-percentage servicer-assignment)) u10000))
      (owner-payment (- actual-payment servicer-fee))
    )
    (asserts! (is-eq tx-sender (get servicer servicer-assignment)) err-not-authorized-servicer)
    (asserts! (get is-active servicer-assignment) err-servicer-not-found)
    (asserts! (not (get is-suspended servicer-profile)) err-servicer-suspended)
    (asserts! (get is-active loan) err-loan-already-paid)
    (asserts! (> payment-amount u0) err-invalid-amount)
    (asserts! (> current-balance u0) err-loan-already-paid)
    
    (try! (stx-transfer? actual-payment (get borrower loan) tx-sender))
    (try! (stx-transfer? owner-payment tx-sender loan-owner))
    
    (map-set loan-details token-id
      (merge loan
        {
          remaining-balance: new-balance,
          payments-made: (+ (get payments-made loan) u1),
          is-active: (> new-balance u0)
        }
      )
    )
    
    (map-set loan-servicers token-id
      (merge servicer-assignment
        {
          payments-processed: (+ (get payments-processed servicer-assignment) u1),
          total-fees-earned: (+ (get total-fees-earned servicer-assignment) servicer-fee)
        }
      )
    )
    
    (map-set servicer-profiles tx-sender
      (merge servicer-profile
        {
          total-payments-processed: (+ (get total-payments-processed servicer-profile) u1)
        }
      )
    )
    
    (map-set payment-history token-id
      (unwrap! (as-max-len? 
        (append payment-history-list 
          {block: stacks-block-height, amount: actual-payment, remaining: new-balance}) 
        u100) 
        err-payment-failed)
    )
    
    (ok {fee-earned: servicer-fee, new-balance: new-balance})
  )
)

(define-public (file-servicer-dispute (token-id uint) (reason uint))
  (let
    (
      (loan-owner (unwrap! (nft-get-owner? student-loan token-id) err-not-token-owner))
      (servicer-assignment (unwrap! (map-get? loan-servicers token-id) err-servicer-not-found))
      (servicer (get servicer servicer-assignment))
    )
    (asserts! (is-eq tx-sender loan-owner) err-not-token-owner)
    (asserts! (get is-active servicer-assignment) err-servicer-not-found)
    (asserts! (<= reason u10) err-invalid-amount)
    
    (map-set servicer-disputes {servicer: servicer, loan-id: token-id}
      {
        dispute-reason: reason,
        filed-block: stacks-block-height,
        is-resolved: false,
        resolution-block: u0
      }
    )
    
    (ok true)
  )
)

(define-public (resolve-servicer-dispute (servicer principal) (loan-id uint) (resolution bool))
  (let
    (
      (dispute (unwrap! (map-get? servicer-disputes {servicer: servicer, loan-id: loan-id}) err-servicer-not-found))
    )
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (not (get is-resolved dispute)) err-servicer-not-found)
    
    (map-set servicer-disputes {servicer: servicer, loan-id: loan-id}
      (merge dispute
        {
          is-resolved: true,
          resolution-block: stacks-block-height
        }
      )
    )
    
    (if (not resolution)
      (map-set servicer-profiles servicer
        (merge (unwrap! (map-get? servicer-profiles servicer) err-servicer-not-found)
          {
            is-suspended: true,
            reputation-score: (let ((current-score (get reputation-score (unwrap! (map-get? servicer-profiles servicer) err-servicer-not-found)))) (if (> current-score u10) (- current-score u10) u0))
          }
        )
      )
      true
    )
    
    (ok resolution)
  )
)

(define-public (update-servicer-settings (max-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= max-fee u2000) err-invalid-servicer-fee)
    
    (var-set max-servicer-fee max-fee)
    
    (ok true)
  )
)

(define-read-only (get-loan-servicer (token-id uint))
  (map-get? loan-servicers token-id)
)

(define-read-only (get-servicer-profile (servicer principal))
  (map-get? servicer-profiles servicer)
)

(define-read-only (get-servicer-dispute (servicer principal) (loan-id uint))
  (map-get? servicer-disputes {servicer: servicer, loan-id: loan-id})
)

(define-read-only (calculate-servicer-fee (token-id uint) (payment-amount uint))
  (match (map-get? loan-servicers token-id)
    assignment-data
      (let
        (
          (fee-percentage (get fee-percentage assignment-data))
        )
        (ok (/ (* payment-amount fee-percentage) u10000))
      )
    (ok u0)
  )
)

(define-read-only (get-servicer-settings)
  (ok {
    max-fee: (var-get max-servicer-fee)
  })
)


