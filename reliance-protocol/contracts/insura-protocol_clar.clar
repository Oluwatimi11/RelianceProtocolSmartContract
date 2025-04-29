;; Insurance Smart Contract
;; Description: A simple insurance contract that allows users to purchase policies,
;; file claims, and receive payouts if claims are approved.

;; Constants
(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-POLICY-DOES-NOT-EXIST (err u101))
(define-constant ERR-PREMIUM-PAYMENT-FAILED (err u102))
(define-constant ERR-DUPLICATE-CLAIM (err u103))
(define-constant ERR-POLICY-LAPSED (err u104))
(define-constant ERR-CLAIM-DOES-NOT-EXIST (err u105))
(define-constant ERR-DISCOUNT-NOT-ELIGIBLE (err u106))
(define-constant ERR-RENEWAL-NOT-FOUND (err u107))

;; Data variables
(define-data-var insurance-company principal tx-sender)
(define-data-var policy-count uint u0)
(define-data-var claim-count uint u0)
(define-data-var company-reserves uint u0)
(define-data-var claim-filing-fee uint u100) ;; in microSTX
(define-data-var no-claims-discount-percentage uint u10) ;; 10% default discount

;; Data maps
(define-map insurance-policies
  uint
  {
    policyholder: principal,
    monthly-premium: uint,
    coverage-limit: uint,
    effective-date: uint,
    expiration-date: uint,
    in-force: bool,
    previous-policy: (optional uint),
    claims-filed: uint
  }
)

(define-map insurance-claims
  uint
  {
    policy-number: uint,
    claim-amount: uint,
    incident-details: (string-ascii 256),
    date-of-loss: uint,
    claim-reviewed: bool,
    claim-approved: bool
  }
)

;; Track policyholder history for no-claims discount
(define-map policyholder-history
  principal
  {
    policies-held: uint,
    total-claims-filed: uint,
    consecutive-claim-free-terms: uint,
    eligible-for-discount: bool
  }
)

;; Public functions

;; Initialize the contract
(define-public (setup-insurance-company)
  (begin
    (asserts! (is-eq tx-sender (var-get insurance-company)) ERR-UNAUTHORIZED-ACCESS)
    (ok true)
  )
)

;; Purchase a new insurance policy
(define-public (enroll-in-coverage (monthly-premium uint) (coverage-limit uint) (term-length uint))
  (let
    (
      (policy-number (var-get policy-count))
      (current-date block-height)
      (expiration-date (+ current-date term-length))
      (policyholder-record (default-to 
        {
          policies-held: u0,
          total-claims-filed: u0,
          consecutive-claim-free-terms: u0,
          eligible-for-discount: false
        }
        (map-get? policyholder-history tx-sender)))
      (discount-eligible (get eligible-for-discount policyholder-record))
      (discount-amount (if discount-eligible
                          (/ (* monthly-premium (var-get no-claims-discount-percentage)) u100)
                          u0))
      (discounted-premium (- monthly-premium discount-amount))
      (total-premium-due (* discounted-premium term-length))
    )
    (asserts! (>= (stx-get-balance tx-sender) total-premium-due) ERR-PREMIUM-PAYMENT-FAILED)
    
    ;; Transfer premium to contract
    (try! (stx-transfer? total-premium-due tx-sender (as-contract tx-sender)))
    
    ;; Update company reserves
    (var-set company-reserves (+ (var-get company-reserves) total-premium-due))
    
    ;; Create new policy
    (map-set insurance-policies policy-number {
      policyholder: tx-sender,
      monthly-premium: discounted-premium,
      coverage-limit: coverage-limit,
      effective-date: current-date,
      expiration-date: expiration-date,
      in-force: true,
      previous-policy: none,
      claims-filed: u0
    })
    
    ;; Update policyholder history
    (map-set policyholder-history tx-sender 
      (merge policyholder-record {
        policies-held: (+ (get policies-held policyholder-record) u1)
      })
    )
    
    ;; Increment policy counter
    (var-set policy-count (+ policy-number u1))
    
    (ok policy-number)
  )
)

;; Renew an existing policy with no-claims discount if eligible
(define-public (renew-policy (old-policy-number uint) (term-length uint))
  (let
    (
      (old-policy (unwrap! (map-get? insurance-policies old-policy-number) ERR-POLICY-DOES-NOT-EXIST))
      (current-date block-height)
      (new-policy-number (var-get policy-count))
      (policyholder-record (default-to 
        {
          policies-held: u0,
          total-claims-filed: u0,
          consecutive-claim-free-terms: u0,
          eligible-for-discount: false
        }
        (map-get? policyholder-history tx-sender)))
      (claims-in-term (get claims-filed old-policy))
      (new-consecutive-terms (if (= claims-in-term u0)
                                (+ (get consecutive-claim-free-terms policyholder-record) u1)
                                u0))
      (discount-eligible (> new-consecutive-terms u0))
      (monthly-premium (get monthly-premium old-policy))
      (coverage-limit (get coverage-limit old-policy))
      (discount-amount (if discount-eligible
                          (/ (* monthly-premium (var-get no-claims-discount-percentage)) u100)
                          u0))
      (discounted-premium (- monthly-premium discount-amount))
      (total-premium-due (* discounted-premium term-length))
      (expiration-date (+ current-date term-length))
    )
    ;; Verify policy ownership
    (asserts! (is-eq (get policyholder old-policy) tx-sender) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Verify payment
    (asserts! (>= (stx-get-balance tx-sender) total-premium-due) ERR-PREMIUM-PAYMENT-FAILED)
    
    ;; Transfer premium to contract
    (try! (stx-transfer? total-premium-due tx-sender (as-contract tx-sender)))
    
    ;; Update company reserves
    (var-set company-reserves (+ (var-get company-reserves) total-premium-due))
    
    ;; Create new policy
    (map-set insurance-policies new-policy-number {
      policyholder: tx-sender,
      monthly-premium: discounted-premium,
      coverage-limit: coverage-limit,
      effective-date: current-date,
      expiration-date: expiration-date,
      in-force: true,
      previous-policy: (some old-policy-number),
      claims-filed: u0
    })
    
    ;; Update old policy status
    (map-set insurance-policies old-policy-number (merge old-policy { in-force: false }))
    
    ;; Update policyholder history with new consecutive claim-free terms
    (map-set policyholder-history tx-sender 
      (merge policyholder-record {
        policies-held: (+ (get policies-held policyholder-record) u1),
        consecutive-claim-free-terms: new-consecutive-terms,
        eligible-for-discount: discount-eligible
      })
    )
    
    ;; Increment policy counter
    (var-set policy-count (+ new-policy-number u1))
    
    (ok new-policy-number)
  )
)

;; File a claim for an existing policy
(define-public (submit-claim (policy-number uint) (claim-amount uint) (incident-details (string-ascii 256)))
  (let
    (
      (policy (unwrap! (map-get? insurance-policies policy-number) ERR-POLICY-DOES-NOT-EXIST))
      (claim-number (var-get claim-count))
      (date-of-loss block-height)
      (policyholder-principal (get policyholder policy))
      (policyholder-record (default-to 
        {
          policies-held: u0,
          total-claims-filed: u0,
          consecutive-claim-free-terms: u0,
          eligible-for-discount: false
        }
        (map-get? policyholder-history policyholder-principal)))
      (current-claims-filed (get claims-filed policy))
    )
    ;; Verify policy ownership
    (asserts! (is-eq policyholder-principal tx-sender) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Verify policy is active
    (asserts! (get in-force policy) ERR-POLICY-DOES-NOT-EXIST)
    
    ;; Verify policy hasn't expired
    (asserts! (<= date-of-loss (get expiration-date policy)) ERR-POLICY-LAPSED)
    
    ;; Verify claim amount is within coverage limit
    (asserts! (<= claim-amount (get coverage-limit policy)) ERR-PREMIUM-PAYMENT-FAILED)
    
    ;; Charge claim filing fee
    (try! (stx-transfer? (var-get claim-filing-fee) tx-sender (as-contract tx-sender)))
    
    ;; Update company reserves
    (var-set company-reserves (+ (var-get company-reserves) (var-get claim-filing-fee)))
    
    ;; Create new claim
    (map-set insurance-claims claim-number {
      policy-number: policy-number,
      claim-amount: claim-amount,
      incident-details: incident-details,
      date-of-loss: date-of-loss,
      claim-reviewed: false,
      claim-approved: false
    })
    
    ;; Update claims filed count on policy
    (map-set insurance-policies policy-number (merge policy {
      claims-filed: (+ current-claims-filed u1)
    }))
    
    ;; Update policyholder history
    (map-set policyholder-history policyholder-principal 
      (merge policyholder-record {
        total-claims-filed: (+ (get total-claims-filed policyholder-record) u1)
      })
    )
    
    ;; Increment claim counter
    (var-set claim-count (+ claim-number u1))
    
    (ok claim-number)
  )
)

;; Process a claim (owner only)
(define-public (adjudicate-claim (claim-number uint) (approve bool))
  (let
    (
      (claim (unwrap! (map-get? insurance-claims claim-number) ERR-CLAIM-DOES-NOT-EXIST))
      (policy-number (get policy-number claim))
      (policy (unwrap! (map-get? insurance-policies policy-number) ERR-POLICY-DOES-NOT-EXIST))
      (payout-amount (get claim-amount claim))
      (beneficiary (get policyholder policy))
    )
    ;; Verify caller is insurance company
    (asserts! (is-eq tx-sender (var-get insurance-company)) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Verify claim hasn't been reviewed yet
    (asserts! (not (get claim-reviewed claim)) ERR-DUPLICATE-CLAIM)
    
    ;; Update claim status
    (map-set insurance-claims claim-number (merge claim {
      claim-reviewed: true,
      claim-approved: approve
    }))
    
    ;; If approved, transfer payment to beneficiary
    (if approve
      (begin
        ;; Verify company has enough reserves
        (asserts! (>= (var-get company-reserves) payout-amount) ERR-PREMIUM-PAYMENT-FAILED)
        
        ;; Transfer funds
        (try! (as-contract (stx-transfer? payout-amount tx-sender beneficiary)))
        
        ;; Update company reserves
        (var-set company-reserves (- (var-get company-reserves) payout-amount))
        
        (ok true)
      )
      (ok false)
    )
  )
)

;; Cancel a policy and refund remaining premium (proportional to time left)
(define-public (cancel-policy (policy-number uint))
  (let
    (
      (policy (unwrap! (map-get? insurance-policies policy-number) ERR-POLICY-DOES-NOT-EXIST))
      (current-date block-height)
      (days-remaining (if (> (get expiration-date policy) current-date)
                           (- (get expiration-date policy) current-date)
                           u0))
      (daily-rate (get monthly-premium policy))
      (refund-amount (* days-remaining daily-rate))
    )
    ;; Verify policy ownership
    (asserts! (is-eq (get policyholder policy) tx-sender) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Verify policy is active
    (asserts! (get in-force policy) ERR-POLICY-DOES-NOT-EXIST)
    
    ;; Update policy status
    (map-set insurance-policies policy-number (merge policy { in-force: false }))
    
    ;; Process refund if applicable
    (if (> refund-amount u0)
      (begin
        ;; Verify contract has enough funds
        (asserts! (>= (var-get company-reserves) refund-amount) ERR-PREMIUM-PAYMENT-FAILED)
        
        ;; Transfer refund
        (try! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)))
        
        ;; Update company reserves
        (var-set company-reserves (- (var-get company-reserves) refund-amount))
        
        (ok refund-amount)
      )
      (ok u0)
    )
  )
)

;; Update no-claims discount percentage (owner only)
(define-public (update-no-claims-discount (new-percentage uint))
  (begin
    ;; Verify caller is insurance company
    (asserts! (is-eq tx-sender (var-get insurance-company)) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Update discount percentage
    (var-set no-claims-discount-percentage new-percentage)
    
    (ok new-percentage)
  )
)

;; Manually set discount eligibility (special cases, owner only)
(define-public (set-discount-eligibility (customer principal) (eligible bool))
  (let
    (
      (customer-record (default-to 
        {
          policies-held: u0,
          total-claims-filed: u0,
          consecutive-claim-free-terms: u0,
          eligible-for-discount: false
        }
        (map-get? policyholder-history customer)))
    )
    ;; Verify caller is insurance company
    (asserts! (is-eq tx-sender (var-get insurance-company)) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Update eligibility
    (map-set policyholder-history customer 
      (merge customer-record {
        eligible-for-discount: eligible
      })
    )
    
    (ok eligible)
  )
)

;; Withdraw funds from contract (owner only)
(define-public (withdraw-company-funds (amount uint))
  (begin
    ;; Verify caller is insurance company
    (asserts! (is-eq tx-sender (var-get insurance-company)) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Verify company has enough reserves
    (asserts! (>= (var-get company-reserves) amount) ERR-PREMIUM-PAYMENT-FAILED)
    
    ;; Transfer funds
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    
    ;; Update company reserves
    (var-set company-reserves (- (var-get company-reserves) amount))
    
    (ok amount)
  )
)

;; Update claim fee (owner only)
(define-public (update-filing-fee (new-fee uint))
  (begin
    ;; Verify caller is insurance company
    (asserts! (is-eq tx-sender (var-get insurance-company)) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Update fee
    (var-set claim-filing-fee new-fee)
    
    (ok new-fee)
  )
)

;; Transfer contract ownership (owner only)
(define-public (transfer-company-ownership (new-owner principal))
  (begin
    ;; Verify caller is insurance company
    (asserts! (is-eq tx-sender (var-get insurance-company)) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Update owner
    (var-set insurance-company new-owner)
    
    (ok true)
  )
)

;; Read-only functions

;; Get policy details
(define-read-only (view-policy-details (policy-number uint))
  (map-get? insurance-policies policy-number)
)

;; Get claim details
(define-read-only (view-claim-details (claim-number uint))
  (map-get? insurance-claims claim-number)
)

;; Get policyholder history and discount status
(define-read-only (view-customer-history (customer principal))
  (map-get? policyholder-history customer)
)

;; Calculate discount amount for a given premium
(define-read-only (calculate-premium-discount (premium uint) (customer principal))
  (let
    (
      (customer-record (default-to 
        {
          policies-held: u0,
          total-claims-filed: u0,
          consecutive-claim-free-terms: u0,
          eligible-for-discount: false
        }
        (map-get? policyholder-history customer)))
      (discount-eligible (get eligible-for-discount customer-record))
      (discount-amount (if discount-eligible
                          (/ (* premium (var-get no-claims-discount-percentage)) u100)
                          u0))
      (discounted-premium (- premium discount-amount))
    )
    {
      original-premium: premium,
      discount-percentage: (if discount-eligible 
                              (var-get no-claims-discount-percentage) 
                              u0),
      discount-amount: discount-amount,
      final-premium: discounted-premium,
      discount-eligible: discount-eligible,
      consecutive-claim-free-terms: (get consecutive-claim-free-terms customer-record)
    }
  )
)

;; Get contract stats
(define-read-only (get-company-statistics)
  {
    total-policies-written: (var-get policy-count),
    total-claims-received: (var-get claim-count),
    current-company-reserves: (var-get company-reserves),
    current-filing-fee: (var-get claim-filing-fee),
    no-claims-discount-rate: (var-get no-claims-discount-percentage)
  }
)