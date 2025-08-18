# 🎓 Student Loan NFT (Loantoken)

A Clarity smart contract that tokenizes student loans as transferable NFTs on the Stacks blockchain, enabling a secondary market for loan obligations.

## 📋 Overview

The Student Loan NFT contract allows educational institutions or lenders to issue student loans as NFTs, which can then be traded on a secondary market. Each NFT represents a loan obligation with specific terms, payment history, and current status.

## ✨ Features

- 🏦 **Loan Issuance**: Create student loans as NFTs with customizable terms
- 💰 **Payment Processing**: Borrowers can make payments directly through the contract
- 🔄 **Secondary Market**: Loan holders can list and sell their loan NFTs
- 📊 **Payment Tracking**: Complete payment history and loan status monitoring
- 📈 **Loan Analytics**: Calculate payment schedules and loan performance metrics

## 🚀 Core Functions

### Loan Management
- `issue-loan`: Create a new student loan NFT
- `make-payment`: Process loan payments from borrowers
- `get-loan-details`: Retrieve comprehensive loan information
- `get-loan-status`: Check current loan performance metrics

### Marketplace Functions
- `list-loan-for-sale`: List a loan NFT for sale
- `buy-loan`: Purchase a listed loan NFT
- `cancel-listing`: Remove a loan from the marketplace

### Analytics & Reporting
- `get-payment-history`: View complete payment records
- `get-borrower-loans`: List all loans for a specific borrower
- `get-contract-stats`: Overall contract statistics

## 📖 Usage Examples

### Issue a New Loan
```clarity
(contract-call? .loantoken issue-loan 'SP1ABC... u50000 u500 u48)
;; Issues a $500 loan at 5% interest for 48 months
```

### Make a Payment
```clarity
(contract-call? .loantoken make-payment u1 u1000)
;; Makes a $10 payment on loan #1
```

### List Loan for Sale
```clarity
(contract-call? .loantoken list-loan-for-sale u1 u45000)
;; Lists loan #1 for sale at $450
```

## 🏗️ Contract Structure

### Data Storage
- **loan-details**: Core loan information and terms
- **loan-listings**: Active marketplace listings
- **borrower-loans**: Mapping of borrowers to their loans
- **payment-history**: Complete payment records per loan

### Key Parameters
- **Principal Amount**: Original loan amount
- **Interest Rate**: Annual percentage rate (basis points)
- **Term**: Loan duration in months
- **Monthly Payment**: Calculated payment amount

## 🔧 Development

### Prerequisites
- Clarinet CLI
- Stacks blockchain testnet access

### Testing
```bash
clarinet test
```

### Deployment
```bash
clarinet deploy --testnet
```

## 🛡️ Security Features

- Owner-only loan issuance
- Payment validation and verification
- Transfer restrictions and ownership checks
- Comprehensive error handling

## 📊 Loan Metrics

The contract automatically calculates:
- Monthly payment amounts
- Payment schedules
- Loan completion percentage
- Delinquency status
- Performance analytics

## 🤝 Contributing

Feel free to submit issues and enhancement requests!

## 📄 License

This project is open source and available under the MIT License.
```

