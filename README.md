# 📄 Insurance Smart Contract - README Update

## 🚀 Overview
This smart contract implements a full-featured decentralized insurance platform on the Stacks blockchain using Clarity. Users can:
- Purchase and renew insurance policies
- File claims
- Benefit from no-claim discounts
- Cancel policies with proportional refunds
- Administer insurance company funds and operations

## 🛠️ Main Features and Functionalities

### 1. Policy Management
- **Create Policy**: Purchase new insurance policies with specified coverage and premium.
- **Renew Policy**: Automatically verify eligibility for discounts, collect payment, update policyholder records, and create a new policy version upon expiration.
- **Cancel Policy**: Allow policyholders to cancel active policies with a proportional refund based on remaining coverage period.

### 2. Claims Handling
- **Submit Claim**: Policyholders can file a claim against their active insurance policies, subject to coverage limits and claim filing fees.
- **Adjudicate Claim**: Administrators can approve or reject claims. Approved claims result in settlement payments directly from company reserves to the policyholder.

### 3. Policyholder Records
- **Policyholder History**: Tracks:
  - Total policies held
  - Total claims filed
  - Consecutive claim-free terms
  - Discount eligibility status
  - Lifetime premiums paid
  - Lifetime claims received
- **Manual Discount Eligibility**: Administrators can manually grant or revoke discount eligibility in special cases.

### 4. Financial Management
- **Company Reserves**: Premiums and claim filing fees increase reserves; settlements and refunds decrease reserves.
- **Withdraw Company Funds**: Company owners can withdraw funds from the contract balance within reserve limits.

### 5. Discounts and Promotions
- **No-Claims Discount**: Automatically apply discounts to premiums for users with consecutive claim-free terms.
- **Update Discount Rate**: Administrators can update the discount percentage up to a configured maximum.

### 6. Administrative Controls
- **Contract Pausing**: Administrators can pause contract functionality in emergencies.
- **Access Controls**: Specific actions (e.g., adjudicating claims, updating discounts) are restricted to administrators or company owners.

### 7. Event Logging
- Events are emitted on critical actions such as:
  - Policy renewal
  - Policy cancellation
  - Claim filing
  - Claim approval/rejection
  - Discount updates
  - Fund withdrawals

---

## 📚 Detailed Public Functions

| Function | Description |
|:---|:---|
| `renew-policy` | Renews an existing policy, applying discounts and updating policyholder history. |
| `submit-claim` | Files a new insurance claim for an active policy. |
| `adjudicate-claim` | Approves or rejects a submitted claim (admin only). |
| `cancel-policy` | Cancels an active policy and refunds unused premium if applicable. |
| `update-no-claims-discount` | Updates the no-claims discount percentage (admin only). |
| `set-discount-eligibility` | Manually set or revoke a policyholder’s discount eligibility (admin only). |
| `withdraw-company-funds` | Withdraws funds from company reserves (owner only). |

---

## ⚙️ Access Control

| Role | Permissions |
|:---|:---|
| **Policyholder** | Create/Renew policies, File claims, Cancel policies |
| **Administrator** | Adjudicate claims, Update discount percentage, Set discount eligibility, Pause/Unpause contract |
| **Company Owner** | Withdraw funds, Full admin rights |

---

## 🛡️ Important Validations and Safeguards
- **Parameter Validation**: All input parameters are strictly validated.
- **Contract Pause Check**: Core functions check that the contract is not paused.
- **Authorization Enforcement**: Sensitive operations restricted to authorized roles.
- **Reserves Management**: Ensures sufficient reserves before any payouts or withdrawals.
- **Event Emissions**: All major operations emit standardized events for off-chain tracking.

---

# 📢 Notes
- This smart contract uses defensive programming with `asserts!`, `unwrap!`, and `try!` patterns to ensure robustness.
- Stringent checks and balances are applied at every step to protect company reserves and policyholder rights.

---
