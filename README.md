# 📚 Readchain - Community Library Registry

A decentralized book sharing platform built on Stacks blockchain that tokenizes books and tracks usage in community libraries.

## 🌟 Features

- 📖 **Book Registration**: Register books with metadata and set rental prices
- 🏛️ **Library Registry**: Libraries can register and manage their collections
- 💰 **Token Economy**: Earn READ tokens for participation and good behavior
- 📝 **Review System**: Rate and review books you've borrowed
- 🔄 **Book Borrowing**: Rent books from other users with automatic payments
- 📊 **User Statistics**: Track borrowing history and reputation scores

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- Stacks wallet for testing

### Installation

```bash
git clone <your-repo>
cd readchain
clarinet check
```

## 📋 Contract Functions

### 🔧 Public Functions

#### `register-book`
Register a new book in the system
```clarity
(register-book "Book Title" "Author Name" "ISBN123" u100)
```

#### `borrow-book`
Borrow a book for specified duration
```clarity
(borrow-book u1 u7) ;; Borrow book ID 1 for 7 days
```

#### `return-book`
Return a borrowed book
```clarity
(return-book u1)
```

#### `register-library`
Register as a community library
```clarity
(register-library "City Public Library")
```

#### `add-review`
Add a review for a book you've borrowed
```clarity
(add-review u1 u5 "Great book!") ;; 5-star rating
```

### 📖 Read-Only Functions

#### `get-book`
Get book information by ID
```clarity
(get-book u1)
```

#### `get-user-stats`
Get user statistics and reputation
```clarity
(get-user-stats 'SP1234...)
```

#### `get-token-balance`
Check READ token balance
```clarity
(get-token-balance 'SP1234...)
```

## 💎 Token Economy

- 🎁 **Book Registration**: Earn 100 READ tokens
- 🏛️ **Library Registration**: Earn 500 READ tokens  
- ⭐ **Book Reviews**: Earn 25 READ tokens
- ✅ **On-time Returns**: Earn 50 READ tokens
- ❌ **Late Returns**: Lose reputation points

## 🎯 Usage Examples

### Register a Book
```clarity
(contract-call? .readchain register-book 
  "The Stacks Handbook" 
  "Stacks Foundation" 
  "978-0123456789" 
  u50)
```

### Borrow a Book
```clarity
(contract-call? .readchain borrow-book u1 u14) ;; 14 days
```

### Check Book Availability
```clarity
(contract-call? .readchain is-book-available u1)
```

## 🔒 Security Features

- ✅ Owner-only functions for sensitive operations
- ✅ Validation for all user inputs
- ✅ Prevention of double-borrowing
- ✅ Automatic payment handling
- ✅ Reputation system for trust

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## 📄 License

This project is licensed under the MIT License.


