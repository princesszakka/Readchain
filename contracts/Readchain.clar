;; traits
(define-trait book-trait
  (
    (get-book-info (uint) (response {title: (string-ascii 100), author: (string-ascii 50), isbn: (string-ascii 20)} uint))
  )
)

;; token definitions
(define-fungible-token read-token)

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-book-unavailable (err u106))
(define-constant err-already-borrowed (err u107))
(define-constant err-not-borrowed (err u108))

;; data vars
(define-data-var next-book-id uint u1)
(define-data-var total-books uint u0)
(define-data-var platform-fee uint u10)

;; data maps
(define-map books
  uint
  {
    title: (string-ascii 100),
    author: (string-ascii 50),
    isbn: (string-ascii 20),
    owner: principal,
    available: bool,
    rental-price: uint,
    total-borrows: uint,
    created-at: uint
  }
)

(define-map book-borrowers
  {book-id: uint, borrower: principal}
  {
    borrowed-at: uint,
    due-date: uint,
    returned: bool
  }
)

(define-map user-stats
  principal
  {
    books-owned: uint,
    books-borrowed: uint,
    total-earned: uint,
    reputation-score: uint
  }
)

(define-map library-registry
  principal
  {
    name: (string-ascii 50),
    registered-at: uint,
    total-books: uint,
    active: bool
  }
)

(define-map book-reviews
  {book-id: uint, reviewer: principal}
  {
    rating: uint,
    review: (string-ascii 500),
    created-at: uint
  }
)

;; public functions
(define-public (register-book (title (string-ascii 100)) (author (string-ascii 50)) (isbn (string-ascii 20)) (rental-price uint))
  (let
    (
      (book-id (var-get next-book-id))
      (current-block stacks-block-height)
    )
    (asserts! (> rental-price u0) err-invalid-amount)
    (try! (ft-mint? read-token u100 tx-sender))
    (map-set books book-id
      {
        title: title,
        author: author,
        isbn: isbn,
        owner: tx-sender,
        available: true,
        rental-price: rental-price,
        total-borrows: u0,
        created-at: current-block
      }
    )
    (var-set next-book-id (+ book-id u1))
    (var-set total-books (+ (var-get total-books) u1))
    (update-user-stats tx-sender u1 u0 u0 u5)
    (ok book-id)
  )
)

(define-public (borrow-book (book-id uint) (duration-days uint))
  (let
    (
      (book (unwrap! (map-get? books book-id) err-not-found))
      (current-block stacks-block-height)
      (due-date (+ current-block (* duration-days u144)))
      (rental-cost (* (get rental-price book) duration-days))
      (platform-cut (/ (* rental-cost (var-get platform-fee)) u100))
      (owner-payment (- rental-cost platform-cut))
    )
    (asserts! (get available book) err-book-unavailable)
    (asserts! (not (is-eq tx-sender (get owner book))) err-unauthorized)
    (asserts! (is-none (map-get? book-borrowers {book-id: book-id, borrower: tx-sender})) err-already-borrowed)
    (try! (ft-transfer? read-token rental-cost tx-sender (get owner book)))
    (map-set books book-id (merge book {available: false, total-borrows: (+ (get total-borrows book) u1)}))
    (map-set book-borrowers {book-id: book-id, borrower: tx-sender}
      {
        borrowed-at: current-block,
        due-date: due-date,
        returned: false
      }
    )
    (update-user-stats tx-sender u0 u1 u0 u2)
    (update-user-stats (get owner book) u0 u0 owner-payment u3)
    (ok true)
  )
)

(define-public (return-book (book-id uint))
  (let
    (
      (book (unwrap! (map-get? books book-id) err-not-found))
      (borrow-info (unwrap! (map-get? book-borrowers {book-id: book-id, borrower: tx-sender}) err-not-borrowed))
      (current-block stacks-block-height)
    )
    (asserts! (not (get returned borrow-info)) err-not-borrowed)
    (map-set books book-id (merge book {available: true}))
    (map-set book-borrowers {book-id: book-id, borrower: tx-sender}
      (merge borrow-info {returned: true})
    )
    (if (<= current-block (get due-date borrow-info))
      (try! (ft-mint? read-token u50 tx-sender))
      (update-user-stats tx-sender u0 u0 u0 (- u2))
    )
    (ok true)
  )
)

(define-public (register-library (name (string-ascii 50)))
  (let
    (
      (current-block stacks-block-height)
    )
    (asserts! (is-none (map-get? library-registry tx-sender)) err-already-exists)
    (map-set library-registry tx-sender
      {
        name: name,
        registered-at: current-block,
        total-books: u0,
        active: true
      }
    )
    (try! (ft-mint? read-token u500 tx-sender))
    (ok true)
  )
)

(define-public (add-review (book-id uint) (rating uint) (review (string-ascii 500)))
  (let
    (
      (book (unwrap! (map-get? books book-id) err-not-found))
      (current-block stacks-block-height)
    )
    (asserts! (and (>= rating u1) (<= rating u5)) err-invalid-amount)
    (asserts! (is-some (map-get? book-borrowers {book-id: book-id, borrower: tx-sender})) err-unauthorized)
    (map-set book-reviews {book-id: book-id, reviewer: tx-sender}
      {
        rating: rating,
        review: review,
        created-at: current-block
      }
    )
    (try! (ft-mint? read-token u25 tx-sender))
    (ok true)
  )
)

(define-public (update-rental-price (book-id uint) (new-price uint))
  (let
    (
      (book (unwrap! (map-get? books book-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get owner book)) err-unauthorized)
    (asserts! (> new-price u0) err-invalid-amount)
    (map-set books book-id (merge book {rental-price: new-price}))
    (ok true)
  )
)

(define-public (transfer-book-ownership (book-id uint) (new-owner principal))
  (let
    (
      (book (unwrap! (map-get? books book-id) err-not-found))
    )
    (asserts! (is-eq tx-sender (get owner book)) err-unauthorized)
    (asserts! (get available book) err-book-unavailable)
    (map-set books book-id (merge book {owner: new-owner}))
    (update-user-stats tx-sender (- u1) u0 u0 u0)
    (update-user-stats new-owner u1 u0 u0 u5)
    (ok true)
  )
)

(define-public (mint-tokens (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ft-mint? read-token amount recipient)
  )
)

;; read only functions
(define-read-only (get-book (book-id uint))
  (map-get? books book-id)
)

(define-read-only (get-user-stats (user principal))
  (default-to
    {books-owned: u0, books-borrowed: u0, total-earned: u0, reputation-score: u0}
    (map-get? user-stats user)
  )
)

(define-read-only (get-borrow-info (book-id uint) (borrower principal))
  (map-get? book-borrowers {book-id: book-id, borrower: borrower})
)

(define-read-only (get-library-info (library principal))
  (map-get? library-registry library)
)

(define-read-only (get-book-review (book-id uint) (reviewer principal))
  (map-get? book-reviews {book-id: book-id, reviewer: reviewer})
)

(define-read-only (get-total-books)
  (var-get total-books)
)

(define-read-only (get-next-book-id)
  (var-get next-book-id)
)

(define-read-only (get-platform-fee)
  (var-get platform-fee)
)

(define-read-only (get-token-balance (user principal))
  (ft-get-balance read-token user)
)

(define-read-only (is-book-available (book-id uint))
  (match (map-get? books book-id)
    book (get available book)
    false
  )
)

(define-read-only (get-book-owner (book-id uint))
  (match (map-get? books book-id)
    book (some (get owner book))
    none
  )
)

;; private functions
(define-private (update-user-stats (user principal) (books-owned-delta uint) (books-borrowed-delta uint) (earned-delta uint) (reputation-delta uint))
  (let
    (
      (current-stats (default-to {books-owned: u0, books-borrowed: u0, total-earned: u0, reputation-score: u0} (map-get? user-stats user)))
    )
    (map-set user-stats user
      {
        books-owned: (+ (get books-owned current-stats) books-owned-delta),
        books-borrowed: (+ (get books-borrowed current-stats) books-borrowed-delta),
        total-earned: (+ (get total-earned current-stats) earned-delta),
        reputation-score: (+ (get reputation-score current-stats) reputation-delta)
      }
    )
  )
)