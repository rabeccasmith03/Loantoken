
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

;; (define-read-only (calculate-monthly-payment (principal uint) (annual-rate uint) (months uint))
;;   (let
;;     (
;;       (monthly-rate (/ annual-rate u1200))
;;       (rate-factor (pow-uint (+ u100 monthly-rate) months))
;;     )
;;     (if (is-eq monthly-rate u0)
;;       (/ principal months)
;;       (/ (* principal (* monthly-rate rate-factor)) (- rate-factor u100))
;;     )
;;   )
;; )

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
