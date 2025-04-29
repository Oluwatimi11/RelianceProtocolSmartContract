;; Insurance Smart Contract
;; Description: A robust insurance contract that allows users to purchase policies,
;; file claims, and receive payouts if claims are approved. Includes a no-claims discount system.
;; Version: 2.0

;; Constants
(define-constant ERR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERR-POLICY-DOES-NOT-EXIST (err u101))
(define-constant ERR-PREMIUM-PAYMENT-FAILED (err u102))
(define-constant ERR-DUPLICATE-CLAIM (err u103))
(define-constant ERR-POLICY-LAPSED (err u104))
(define-constant ERR-CLAIM-DOES-NOT-EXIST (err u105))
(define-constant ERR-DISCOUNT-NOT-ELIGIBLE (err u106))
(define-constant ERR-RENEWAL-NOT-FOUND (err u107))
(define-constant ERR-COVERAGE-LIMIT-EXCEEDED (err u108))
(define-constant ERR-INVALID-PARAMETERS (err u109))
(define-constant ERR-CONTRACT-PAUSED (err u110))
(define-constant ERR-SELF-TRANSFER (err u111))
(define-constant ERR-ZERO-AMOUNT (err u112))
(define-constant ERR-ALREADY-INITIALIZED (err u113))
(define-constant ERR-TERM-TOO-SHORT (err u114))
(define-constant ERR-INVALID-STATE-TRANSITION (err u115))

;; Data variables
(define-data-var insurance-company principal tx-sender)
(define-data-var policy-count uint u0)
(define-data-var claim-count uint u0)
(define-data-var company-reserves uint u0)
(define-data-var claim-filing-fee uint u100) ;; in microSTX
(define-data-var no-claims-discount-percentage uint u10) ;; 10% default discount
(define-data-var contract-initialized bool false)
(define-data-var contract-paused bool false)
(define-data-var min-premium uint u10) ;; Minimum premium allowed
(define-data-var min-term-length uint u30) ;; Minimum term length in blocks
(define-data-var max-term-length uint u52560) ;; Maximum term length (approximately 1 year in blocks)
(define-data-var max-discount-percentage uint u50) ;; Maximum discount percentage allowed (50%)

;; Data maps
(define-map insurance-policies
  uint
  {
    policyholder: principal,
    monthly-premium: uint,
    coverage-limit: uint,
    effective-date: uint,
    expiration-date: uint,
    status: (string-ascii 20), ;; "active", "expired", "cancelled", "claimed"
    previous-policy: (optional uint),
    claims-filed: uint,
    total-premium-paid: uint
  }
)

(define-map insurance-claims
  uint
  {
    policy-number: uint,
    claim-amount: uint,
    incident-details: (string-ascii 256),
    date-of-loss: uint,
    status: (string-ascii 20), ;; "pending", "approved", "rejected"
    settlement-date: (optional uint),
    settlement-amount: (optional uint)
  }
)

;; Track policyholder history for no-claims discount
(define-map policyholder-history
  principal
  {
    policies-held: uint,
    total-claims-filed: uint,
    consecutive-claim-free-terms: uint,
    eligible-for-discount: bool,
    last-activity-date: uint,
    lifetime-premiums-paid: uint,
    lifetime-claims-received: uint
  }
)

;; Authorized administrators list
(define-map administrators
  principal
  bool
)

;; Events for indexing and tracking
(define-data-var event-counter uint u0)

(define-map events
  uint
  {
    event-type: (string-ascii 30),
    event-data: (string-ascii 256),
    timestamp: uint,
    related-entity: (optional uint),
    principal-involved: principal
  }
)

;; Private functions

;; Log an event
(define-private (log-event (event-type (string-ascii 30)) 
                          (event-data (string-ascii 256)) 
                          (related-entity (optional uint)) 
                          (principal-involved principal))
  (let
    (
      (event-id (var-get event-counter))
    )
    (map-set events event-id {
      event-type: event-type,
      event-data: event-data,
      timestamp: block-height,
      related-entity: related-entity,
      principal-involved: principal-involved
    })
    (var-set event-counter (+ event-id u1))
    event-id
  )
)

;; Verify administrator privileges
(define-private (is-administrator (user principal))
  (or
    (is-eq user (var-get insurance-company))
    (default-to false (map-get? administrators user))
  )
)

;; Calculate discounted premium
(define-private (calculate-discounted-premium (premium uint) (customer principal))
  (let
    (
      (customer-record (default-to 
        {
          policies-held: u0,
          total-claims-filed: u0,
          consecutive-claim-free-terms: u0,
          eligible-for-discount: false,
          last-activity-date: u0,
          lifetime-premiums-paid: u0,
          lifetime-claims-received: u0
        }
        (map-get? policyholder-history customer)))
      (discount-eligible (get eligible-for-discount customer-record))
      (discount-amount (if discount-eligible
                          (/ (* premium (var-get no-claims-discount-percentage)) u100)
                          u0))
    )
    (- premium discount-amount)
  )
)

;; Check if policy is active
(define-private (is-policy-active (policy-number uint))
  (let
    (
      (policy (map-get? insurance-policies policy-number))
    )
    (and
      (is-some policy)
      (is-eq (get status (unwrap-panic policy)) "active")
      (<= block-height (get expiration-date (unwrap-panic policy)))
    )
  )
)

;; Check if policy is eligible for renewal
(define-private (is-policy-renewable (policy-number uint))
  (let
    (
      (policy (map-get? insurance-policies policy-number))
      (current-date block-height)
    )
    (and
      (is-some policy)
      (is-eq (get status (unwrap-panic policy)) "active")
      (<= current-date (+ (get expiration-date (unwrap-panic policy)) u1440)) ;; Within 10 days of expiration
    )
  )
)

;; Update policyholder stats
(define-private (update-policyholder-stats (customer principal) 
                                          (premium-paid uint) 
                                          (claim-received uint))
  (let
    (
      (customer-record (default-to 
        {
          policies-held: u0,
          total-claims-filed: u0,
          consecutive-claim-free-terms: u0,
          eligible-for-discount: false,
          last-activity-date: u0,
          lifetime-premiums-paid: u0,
          lifetime-claims-received: u0
        }
        (map-get? policyholder-history customer)))
    )
    (map-set policyholder-history customer 
      (merge customer-record {
        last-activity-date: block-height,
        lifetime-premiums-paid: (+ (get lifetime-premiums-paid customer-record) premium-paid),
        lifetime-claims-received: (+ (get lifetime-claims-received customer-record) claim-received)
      })
    )
  )
)

;; Public functions

;; Initialize the contract
(define-public (setup-insurance-company)
  (begin
    (asserts! (not (var-get contract-initialized)) ERR-ALREADY-INITIALIZED)
    (var-set contract-initialized true)
    (map-set administrators tx-sender true)
    (log-event "contract-initialized" "Insurance contract initialized" none tx-sender)
    (ok true)
  )
)

;; Emergency pause/unpause contract
(define-public (toggle-contract-pause)
  (begin
    (asserts! (is-administrator tx-sender) ERR-UNAUTHORIZED-ACCESS)
    (var-set contract-paused (not (var-get contract-paused)))
    (log-event "contract-status-change" 
              (if (var-get contract-paused) "Contract paused" "Contract unpaused")
              none 
              tx-sender)
    (ok (var-get contract-paused))
  )
)

;; Add or remove an administrator
(define-public (manage-administrator (admin principal) (is-admin bool))
  (begin
    (asserts! (is-eq tx-sender (var-get insurance-company)) ERR-UNAUTHORIZED-ACCESS)
    (map-set administrators admin is-admin)
    (log-event "admin-change" 
              (if is-admin "Administrator added" "Administrator removed") 
              none 
              admin)
    (ok is-admin)
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
          eligible-for-discount: false,
          last-activity-date: u0,
          lifetime-premiums-paid: u0,
          lifetime-claims-received: u0
        }
        (map-get? policyholder-history tx-sender)))
      (discounted-premium (calculate-discounted-premium monthly-premium tx-sender))
      (total-premium-due (* discounted-premium term-length))
    )
    ;; Validate contract is not paused
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    
    ;; Validate parameters
    (asserts! (>= monthly-premium (var-get min-premium)) ERR-INVALID-PARAMETERS)
    (asserts! (and (>= term-length (var-get min-term-length)) 
                  (<= term-length (var-get max-term-length))) ERR-TERM-TOO-SHORT)
    (asserts! (> coverage-limit u0) ERR-INVALID-PARAMETERS)
    
    ;; Verify payment
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
      status: "active",
      previous-policy: none,
      claims-filed: u0,
      total-premium-paid: total-premium-due
    })
    
    ;; Update policyholder history
    (map-set policyholder-history tx-sender 
      (merge policyholder-record {
        policies-held: (+ (get policies-held policyholder-record) u1),
        last-activity-date: current-date,
        lifetime-premiums-paid: (+ (get lifetime-premiums-paid policyholder-record) total-premium-due)
      })
    )
    
    ;; Increment policy counter
    (var-set policy-count (+ policy-number u1))
    
    ;; Log event
    (log-event "policy-created" "New policy created" (some policy-number) tx-sender)
    
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
          eligible-for-discount: false,
          last-activity-date: u0,
          lifetime-premiums-paid: u0,
          lifetime-claims-received: u0
        }
        (map-get? policyholder-history tx-sender)))
      (claims-in-term (get claims-filed old-policy))
      (new-consecutive-terms (if (is-eq claims-in-term u0)
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
    ;; Validate contract is not paused
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    
    ;; Verify policy ownership
    (asserts! (is-eq (get policyholder old-policy) tx-sender) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Verify policy is renewable
    (asserts! (is-policy-renewable old-policy-number) ERR-INVALID-STATE-TRANSITION)
    
    ;; Validate parameters
    (asserts! (and (>= term-length (var-get min-term-length)) 
                  (<= term-length (var-get max-term-length))) ERR-TERM-TOO-SHORT)
    
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
      status: "active",
      previous-policy: (some old-policy-number),
      claims-filed: u0,
      total-premium-paid: total-premium-due
    })
    
    ;; Update old policy status
    (map-set insurance-policies old-policy-number (merge old-policy { status: "expired" }))
    
    ;; Update policyholder history with new consecutive claim-free terms
    (map-set policyholder-history tx-sender 
      (merge policyholder-record {
        policies-held: (+ (get policies-held policyholder-record) u1),
        consecutive-claim-free-terms: new-consecutive-terms,
        eligible-for-discount: discount-eligible,
        last-activity-date: current-date,
        lifetime-premiums-paid: (+ (get lifetime-premiums-paid policyholder-record) total-premium-due)
      })
    )
    
    ;; Increment policy counter
    (var-set policy-count (+ new-policy-number u1))
    
    ;; Log event
    (log-event "policy-renewed" "Policy renewed" (some new-policy-number) tx-sender)
    
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
          eligible-for-discount: false,
          last-activity-date: u0,
          lifetime-premiums-paid: u0,
          lifetime-claims-received: u0
        }
        (map-get? policyholder-history policyholder-principal)))
      (current-claims-filed (get claims-filed policy))
    )
    ;; Validate contract is not paused
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    
    ;; Verify policy ownership
    (asserts! (is-eq policyholder-principal tx-sender) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Verify policy is active
    (asserts! (is-eq (get status policy) "active") ERR-POLICY-DOES-NOT-EXIST)
    
    ;; Verify policy hasn't expired
    (asserts! (<= date-of-loss (get expiration-date policy)) ERR-POLICY-LAPSED)
    
    ;; Verify claim amount is within coverage limit
    (asserts! (<= claim-amount (get coverage-limit policy)) ERR-COVERAGE-LIMIT-EXCEEDED)
    
    ;; Validate parameters
    (asserts! (> claim-amount u0) ERR-INVALID-PARAMETERS)
    
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
      status: "pending",
      settlement-date: none,
      settlement-amount: none
    })
    
    ;; Update claims filed count on policy
    (map-set insurance-policies policy-number (merge policy {
      claims-filed: (+ current-claims-filed u1)
    }))
    
    ;; Update policyholder history
    (map-set policyholder-history policyholder-principal 
      (merge policyholder-record {
        total-claims-filed: (+ (get total-claims-filed policyholder-record) u1),
        last-activity-date: date-of-loss
      })
    )
    
    ;; Increment claim counter
    (var-set claim-count (+ claim-number u1))
    
    ;; Log event
    (log-event "claim-filed" "New claim submitted" (some claim-number) tx-sender)
    
    (ok claim-number)
  )
)

;; Process a claim (owner only)
(define-public (adjudicate-claim (claim-number uint) (approve bool) (settlement-amount uint))
  (let
    (
      (claim (unwrap! (map-get? insurance-claims claim-number) ERR-CLAIM-DOES-NOT-EXIST))
      (policy-number (get policy-number claim))
      (policy (unwrap! (map-get? insurance-policies policy-number) ERR-POLICY-DOES-NOT-EXIST))
      (payout-amount (if approve settlement-amount u0))
      (beneficiary (get policyholder policy))
      (requested-amount (get claim-amount claim))
    )
    ;; Validate contract is not paused
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    
    ;; Verify caller is an administrator
    (asserts! (is-administrator tx-sender) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Verify claim hasn't been processed yet
    (asserts! (is-eq (get status claim) "pending") ERR-INVALID-STATE-TRANSITION)
    
    ;; Verify settlement amount is valid if approved
    (asserts! (or (not approve) 
                (and (> settlement-amount u0) 
                     (<= settlement-amount requested-amount)
                     (<= settlement-amount (get coverage-limit policy)))) 
              ERR-INVALID-PARAMETERS)
    
    ;; Update claim status
    (map-set insurance-claims claim-number (merge claim {
      status: (if approve "approved" "rejected"),
      settlement-date: (some block-height),
      settlement-amount: (if approve (some settlement-amount) none)
    }))
    
    ;; If approved, transfer payment to beneficiary and update policy
    (if approve
      (begin
        ;; Verify company has enough reserves
        (asserts! (>= (var-get company-reserves) payout-amount) ERR-PREMIUM-PAYMENT-FAILED)
        
        ;; Update policy status to claimed if fully paid out
        (if (is-eq payout-amount (get coverage-limit policy))
          (map-set insurance-policies policy-number (merge policy { status: "claimed" }))
          (map-set insurance-policies policy-number policy)
        )
        
        ;; Transfer funds
        (try! (as-contract (stx-transfer? payout-amount tx-sender beneficiary)))
        
        ;; Update company reserves
        (var-set company-reserves (- (var-get company-reserves) payout-amount))
        
        ;; Update policyholder stats
        (update-policyholder-stats beneficiary u0 payout-amount)
        
        ;; Log event
        (log-event "claim-approved" "Claim approved with settlement" (some claim-number) tx-sender)
        
        (ok true)
      )
      (begin
        ;; Log event
        (log-event "claim-rejected" "Claim rejected" (some claim-number) tx-sender)
        
        (ok false)
      )
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
      (total-term (- (get expiration-date policy) (get effective-date policy)))
      (refund-amount (if (> total-term u0)
                        (/ (* (get total-premium-paid policy) days-remaining) total-term)
                        u0))
    )
    ;; Validate contract is not paused
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    
    ;; Verify policy ownership
    (asserts! (is-eq (get policyholder policy) tx-sender) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Verify policy is active
    (asserts! (is-eq (get status policy) "active") ERR-INVALID-STATE-TRANSITION)
    
    ;; Update policy status
    (map-set insurance-policies policy-number (merge policy { status: "cancelled" }))
    
    ;; Process refund if applicable
    (if (> refund-amount u0)
      (begin
        ;; Verify contract has enough funds
        (asserts! (>= (var-get company-reserves) refund-amount) ERR-PREMIUM-PAYMENT-FAILED)
        
        ;; Transfer refund
        (try! (as-contract (stx-transfer? refund-amount tx-sender tx-sender)))
        
        ;; Update company reserves
        (var-set company-reserves (- (var-get company-reserves) refund-amount))
        
        ;; Log event
        (log-event "policy-cancelled" "Policy cancelled with refund" (some policy-number) tx-sender)
        
        (ok refund-amount)
      )
      (begin
        ;; Log event
        (log-event "policy-cancelled" "Policy cancelled without refund" (some policy-number) tx-sender)
        
        (ok u0)
      )
    )
  )
)

;; Update no-claims discount percentage (owner only)
(define-public (update-no-claims-discount (new-percentage uint))
  (begin
    ;; Validate contract is not paused
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    
    ;; Verify caller is an administrator
    (asserts! (is-administrator tx-sender) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Verify percentage is valid
    (asserts! (<= new-percentage (var-get max-discount-percentage)) ERR-INVALID-PARAMETERS)
    
    ;; Update discount percentage
    (var-set no-claims-discount-percentage new-percentage)
    
    ;; Log event
    (log-event "discount-updated" 
              (concat "No-claims discount updated to " (int-to-ascii new-percentage))
              none 
              tx-sender)
    
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
          eligible-for-discount: false,
          last-activity-date: u0,
          lifetime-premiums-paid: u0,
          lifetime-claims-received: u0
        }
        (map-get? policyholder-history customer)))
    )
    ;; Validate contract is not paused
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    
    ;; Verify caller is an administrator
    (asserts! (is-administrator tx-sender) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Update eligibility
    (map-set policyholder-history customer 
      (merge customer-record {
        eligible-for-discount: eligible,
        last-activity-date: block-height
      })
    )
    
    ;; Log event
    (log-event "eligibility-changed" 
              (if eligible "Discount eligibility granted" "Discount eligibility revoked")
              none 
              customer)
    
    (ok eligible)
  )
)

;; Withdraw funds from contract (owner only)
(define-public (withdraw-company-funds (amount uint))
  (begin
    ;; Validate contract is not paused
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    
    ;; Verify caller is company owner
    (asserts! (is-eq tx-sender (var-get insurance-company)) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Validate parameters
    (asserts! (> amount u0) ERR-ZERO-AMOUNT)
    
    ;; Verify company has enough reserves
    (asserts! (>= (var-get company-reserves) amount) ERR-PREMIUM-PAYMENT-FAILED)
    
    ;; Transfer funds
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    
    ;; Update company reserves
    (var-set company-reserves (- (var-get company-reserves) amount))
    
    ;; Log event
    (log-event "funds-withdrawn" 
              (concat "Company funds withdrawn: " (int-to-ascii amount))
              none 
              tx-sender)
    
    (ok amount)
  )
)

;; Update claim fee (owner only)
(define-public (update-filing-fee (new-fee uint))
  (begin
    ;; Validate contract is not paused
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    
    ;; Verify caller is an administrator
    (asserts! (is-administrator tx-sender) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Update fee
    (var-set claim-filing-fee new-fee)
    
    ;; Log event
    (log-event "fee-updated" 
              (concat "Claim filing fee updated to " (int-to-ascii new-fee))
              none 
              tx-sender)
    
    (ok new-fee)
  )
)

;; Transfer contract ownership (owner only)
(define-public (transfer-company-ownership (new-owner principal))
  (begin
    ;; Validate contract is not paused
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    
    ;; Verify caller is company owner
    (asserts! (is-eq tx-sender (var-get insurance-company)) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Prevent transferring to yourself
    (asserts! (not (is-eq new-owner tx-sender)) ERR-SELF-TRANSFER)
    
    ;; Update owner
    (var-set insurance-company new-owner)
    
    ;; Log event
    (log-event "ownership-transferred" 
              "Company ownership transferred"
              none 
              new-owner)
    
    (ok true)
  )
)

;; Update contract configuration (owner only)
(define-public (update-contract-configuration 
               (new-min-premium uint) 
               (new-min-term uint) 
               (new-max-term uint)
               (new-max-discount uint))
  (begin
    ;; Validate contract is not paused
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    
    ;; Verify caller is an administrator
    (asserts! (is-administrator tx-sender) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Validate parameters
    (asserts! (> new-min-premium u0) ERR-INVALID-PARAMETERS)
    (asserts! (and (> new-min-term u0) (< new-min-term new-max-term)) ERR-INVALID-PARAMETERS)
    (asserts! (<= new-max-discount u100) ERR-INVALID-PARAMETERS)
    
    ;; Update configuration
    (var-set min-premium new-min-premium)
    (var-set min-term-length new-min-term)
    (var-set max-term-length new-max-term)
    (var-set max-discount-percentage new-max-discount)
    
    ;; Log event
    (log-event "config-updated" 
              "Contract configuration updated"
              none 
              tx-sender)
    
    (ok true)
  )
)

;; Get policy details
(define-public (get-policy-details (policy-number uint))
  (let
    (
      (policy (map-get? insurance-policies policy-number))
    )
    (if (is-some policy)
      (ok (unwrap-panic policy))
      (err ERR-POLICY-DOES-NOT-EXIST)
    )
  )
)

;; Get claim details
(define-public (get-claim-details (claim-number uint))
  (let
    (
      (claim (map-get? insurance-claims claim-number))
    )
    (if (is-some claim)
      (ok (unwrap-panic claim))
      (err ERR-CLAIM-DOES-NOT-EXIST)
    )
  )
)

;; Get policyholder's history and discount eligibility
(define-public (get-discount-info (policyholder principal))
  (let
    (
      (history (map-get? policyholder-history policyholder))
    )
    (if (is-some history)
      (ok (unwrap-panic history))
      (err ERR-DISCOUNT-NOT-ELIGIBLE)
    )
  )
)

;; Deposit additional funds to company reserves
(define-public (deposit-to-reserves (amount uint))
  (begin
    ;; Validate contract is not paused
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    
    ;; Validate parameters
    (asserts! (> amount u0) ERR-ZERO-AMOUNT)
    
    ;; Transfer funds to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Update company reserves
    (var-set company-reserves (+ (var-get company-reserves) amount))
    
    ;; Log event
    (log-event "funds-deposited" 
              (concat "Funds deposited to reserves: " (int-to-ascii amount))
              none 
              tx-sender)
    
    (ok amount)
  )
)

;; Get contract statistics
(define-public (get-contract-stats)
  (ok {
    policy-count: (var-get policy-count),
    claim-count: (var-get claim-count),
    company-reserves: (var-get company-reserves),
    min-premium: (var-get min-premium),
    min-term-length: (var-get min-term-length),
    max-term-length: (var-get max-term-length),
    no-claims-discount: (var-get no-claims-discount-percentage),
    max-discount: (var-get max-discount-percentage),
    claim-filing-fee: (var-get claim-filing-fee),
    contract-paused: (var-get contract-paused)
  })
)

;; Extend an existing policy
(define-public (extend-policy (policy-number uint) (additional-term uint))
  (let
    (
      (policy (unwrap! (map-get? insurance-policies policy-number) ERR-POLICY-DOES-NOT-EXIST))
      (current-date block-height)
      (current-expiration (get expiration-date policy))
      (new-expiration (+ current-expiration additional-term))
      (premium-per-block (get monthly-premium policy))
      (additional-premium (* premium-per-block additional-term))
    )
    ;; Validate contract is not paused
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    
    ;; Verify policy ownership
    (asserts! (is-eq (get policyholder policy) tx-sender) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Verify policy is active
    (asserts! (is-eq (get status policy) "active") ERR-INVALID-STATE-TRANSITION)
    
    ;; Verify policy hasn't expired
    (asserts! (>= current-expiration current-date) ERR-POLICY-LAPSED)
    
    ;; Validate parameters
    (asserts! (> additional-term u0) ERR-INVALID-PARAMETERS)
    (asserts! (<= (- new-expiration current-date) (var-get max-term-length)) ERR-TERM-TOO-SHORT)
    
    ;; Verify payment
    (asserts! (>= (stx-get-balance tx-sender) additional-premium) ERR-PREMIUM-PAYMENT-FAILED)
    
    ;; Transfer premium to contract
    (try! (stx-transfer? additional-premium tx-sender (as-contract tx-sender)))
    
    ;; Update company reserves
    (var-set company-reserves (+ (var-get company-reserves) additional-premium))
    
    ;; Update policy
    (map-set insurance-policies policy-number (merge policy {
      expiration-date: new-expiration,
      total-premium-paid: (+ (get total-premium-paid policy) additional-premium)
    }))
    
    ;; Update policyholder history
    (update-policyholder-stats tx-sender additional-premium u0)
    
    ;; Log event
    (log-event "policy-extension" 
  (concat 
    (concat "Policy extended by " (int-to-ascii additional-term)) 
    " blocks"
  )
  none
  tx-sender
)
    
    (ok new-expiration)
  )
)

;; Upgrade coverage amount for existing policy
(define-public (upgrade-coverage (policy-number uint) (new-coverage-limit uint))
  (let
    (
      (policy (unwrap! (map-get? insurance-policies policy-number) ERR-POLICY-DOES-NOT-EXIST))
      (current-date block-height)
      (current-coverage (get coverage-limit policy))
      (coverage-increase (- new-coverage-limit current-coverage))
      (remaining-blocks (- (get expiration-date policy) current-date))
      (current-premium (get monthly-premium policy))
      ;; Simplified calculation - premium increases proportionally with coverage
      (new-premium (/ (* current-premium new-coverage-limit) current-coverage))
      (premium-increase (- new-premium current-premium))
      (additional-cost (* premium-increase remaining-blocks))
    )
    ;; Validate contract is not paused
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    
    ;; Verify policy ownership
    (asserts! (is-eq (get policyholder policy) tx-sender) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Verify policy is active
    (asserts! (is-eq (get status policy) "active") ERR-INVALID-STATE-TRANSITION)
    
    ;; Verify policy hasn't expired
    (asserts! (>= (get expiration-date policy) current-date) ERR-POLICY-LAPSED)
    
    ;; Validate parameters
    (asserts! (> new-coverage-limit current-coverage) ERR-INVALID-PARAMETERS)
    
    ;; Verify payment for increased coverage
    (asserts! (>= (stx-get-balance tx-sender) additional-cost) ERR-PREMIUM-PAYMENT-FAILED)
    
    ;; Transfer additional premium to contract
    (try! (stx-transfer? additional-cost tx-sender (as-contract tx-sender)))
    
    ;; Update company reserves
    (var-set company-reserves (+ (var-get company-reserves) additional-cost))
    
    ;; Update policy
    (map-set insurance-policies policy-number (merge policy {
      coverage-limit: new-coverage-limit,
      monthly-premium: new-premium,
      total-premium-paid: (+ (get total-premium-paid policy) additional-cost)
    }))
    
    ;; Update policyholder history
    (update-policyholder-stats tx-sender additional-cost u0)
    
    ;; Log event
    (log-event "coverage-upgraded" 
              (concat "Policy coverage upgraded to " (int-to-ascii new-coverage-limit))
              (some policy-number) 
              tx-sender)
    
    (ok new-coverage-limit)
  )
)

;; Check if a principal is an administrator
(define-public (check-administrator-status (user principal))
  (ok (is-administrator user))
)

;; Calculate premium quote with no-claims discount
(define-public (calculate-premium-quote (base-premium uint) (term-length uint))
  (let
    (
      (discounted-premium (calculate-discounted-premium base-premium tx-sender))
      (total-premium (* discounted-premium term-length))
    )
    (ok {
      base-premium: base-premium,
      discounted-premium: discounted-premium,
      term-length: term-length,
      total-premium: total-premium
    })
  )
)

;; View function to get policy count
(define-read-only (get-policy-count)
  (var-get policy-count)
)

;; View function to get claim count
(define-read-only (get-claim-count)
  (var-get claim-count)
)

;; View function to get company reserves
(define-read-only (get-company-reserves)
  (var-get company-reserves)
)

;; View function to check if contract is paused
(define-read-only (is-contract-paused)
  (var-get contract-paused)
)

;; View function to get company owner
(define-read-only (get-company-owner)
  (var-get insurance-company)
)

;; View function to check if a policy exists
(define-read-only (does-policy-exist (policy-number uint))
  (is-some (map-get? insurance-policies policy-number))
)

;; View function to get the status of a policy
(define-read-only (get-policy-status (policy-number uint))
  (match (map-get? insurance-policies policy-number)
    policy (ok (get status policy))
    (err ERR-POLICY-DOES-NOT-EXIST)
  )
)

;; Batch process expired policies
(define-public (batch-expire-policies (policy-numbers (list 10 uint)))
  (let
    (
      (current-date block-height)
      (processed-count (fold process-expiration policy-numbers u0))
    )
    ;; Validate contract is not paused
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    
    ;; Verify caller is an administrator
    (asserts! (is-administrator tx-sender) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Log event
    (log-event "batch-expiration" 
              (concat (concat "Processed " (int-to-ascii processed-count)) " expired policies")
              none 
              tx-sender)
    
    (ok processed-count)
  )
)

;; Helper function for batch expiration
(define-private (process-expiration (policy-number uint) (processed uint))
  (match (map-get? insurance-policies policy-number)
    policy 
      (if (and 
            (is-eq (get status policy) "active")
            (< (get expiration-date policy) block-height)
          )
        (begin
          (map-set insurance-policies policy-number (merge policy { status: "expired" }))
          (+ processed u1)
        )
        processed
      )
    processed
  )
)

;; Generate a claim report
(define-public (generate-claim-report (claim-number uint))
  (let
    (
      (claim (unwrap! (map-get? insurance-claims claim-number) ERR-CLAIM-DOES-NOT-EXIST))
      (policy-number (get policy-number claim))
      (policy (unwrap! (map-get? insurance-policies policy-number) ERR-POLICY-DOES-NOT-EXIST))
    )
    ;; Validate contract is not paused
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    
    ;; Verify caller is an administrator or policyholder
    (asserts! (or 
               (is-administrator tx-sender)
               (is-eq tx-sender (get policyholder policy))
              ) 
              ERR-UNAUTHORIZED-ACCESS)
    
    ;; Return report data
    (ok {
      claim-number: claim-number,
      policy-number: policy-number,
      policyholder: (get policyholder policy),
      claim-amount: (get claim-amount claim),
      incident-details: (get incident-details claim),
      date-of-loss: (get date-of-loss claim),
      status: (get status claim),
      settlement-date: (get settlement-date claim),
      settlement-amount: (get settlement-amount claim),
      coverage-limit: (get coverage-limit policy),
      policy-status: (get status policy)
    })
  )
)

;; Emergency update of a claim status (for error correction)
(define-public (emergency-update-claim (claim-number uint) (new-status (string-ascii 20)))
  (let
    (
      (claim (unwrap! (map-get? insurance-claims claim-number) ERR-CLAIM-DOES-NOT-EXIST))
    )
    ;; Validate contract is not paused
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    
    ;; Verify caller is company owner (restricted function)
    (asserts! (is-eq tx-sender (var-get insurance-company)) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Validate status
    (asserts! (or 
               (is-eq new-status "pending")
               (is-eq new-status "approved")
               (is-eq new-status "rejected")
              ) 
              ERR-INVALID-PARAMETERS)
    
    ;; Update claim status
    (map-set insurance-claims claim-number (merge claim {
      status: new-status
    }))
    
    ;; Log event
    (log-event "emergency-update" 
              (concat "Emergency claim status update to " new-status)
              (some claim-number) 
              tx-sender)
    
    (ok true)
  )
)

;; Deposit funds to special reserve for catastrophic events
(define-data-var catastrophe-reserves uint u0)

(define-public (deposit-to-catastrophe-fund (amount uint))
  (begin
    ;; Validate contract is not paused
    (asserts! (not (var-get contract-paused)) ERR-CONTRACT-PAUSED)
    
    ;; Verify caller is an administrator
    (asserts! (is-administrator tx-sender) ERR-UNAUTHORIZED-ACCESS)
    
    ;; Validate parameters
    (asserts! (> amount u0) ERR-ZERO-AMOUNT)
    
    ;; Transfer from company reserves to catastrophe reserves
    (asserts! (>= (var-get company-reserves) amount) ERR-PREMIUM-PAYMENT-FAILED)
    
    ;; Update reserves
    (var-set company-reserves (- (var-get company-reserves) amount))
    (var-set catastrophe-reserves (+ (var-get catastrophe-reserves) amount))
    
    ;; Log event
    (log-event "catastrophe-fund" 
              (concat "Added to catastrophe fund: " (int-to-ascii amount))
              none 
              tx-sender)
    
    (ok amount)
  )
)

;; View catastrophe reserves
(define-read-only (get-catastrophe-reserves)
  (var-get catastrophe-reserves)
)

;; Check if a contract has been initialized
(define-read-only (is-contract-initialized)
  (var-get contract-initialized)
)