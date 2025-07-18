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
(define-constant err-invalid-genre (err u109))
(define-constant err-list-not-found (err u110))
(define-constant err-already-in-list (err u111))
(define-constant err-not-in-list (err u112))
(define-constant err-list-full (err u113))

;; data vars
(define-data-var next-book-id uint u1)
(define-data-var total-books uint u0)
(define-data-var platform-fee uint u10)
(define-data-var next-list-id uint u1)
(define-data-var total-reading-lists uint u0)

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
    created-at: uint,
    genre: (string-ascii 30),
    popularity-score: uint,
    recommendation-count: uint
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

(define-map reading-lists
  uint
  {
    owner: principal,
    name: (string-ascii 50),
    description: (string-ascii 200),
    created-at: uint,
    is-public: bool,
    book-count: uint,
    followers: uint
  }
)

(define-map reading-list-books
  {list-id: uint, book-id: uint}
  {
    added-at: uint,
    position: uint,
    notes: (string-ascii 300)
  }
)

(define-map genre-stats
  (string-ascii 30)
  {
    total-books: uint,
    total-borrows: uint,
    avg-rating: uint,
    trending-score: uint
  }
)

(define-map user-genre-preferences
  {user: principal, genre: (string-ascii 30)}
  {
    books-read: uint,
    last-activity: uint,
    preference-score: uint
  }
)

(define-map book-recommendations
  {recommender: principal, book-id: uint, recommended-to: principal}
  {
    created-at: uint,
    reason: (string-ascii 200),
    accepted: bool
  }
)

(define-map list-followers
  {list-id: uint, follower: principal}
  {
    followed-at: uint,
    notifications: bool
  }
)

;; public functions
(define-public (register-book (title (string-ascii 100)) (author (string-ascii 50)) (isbn (string-ascii 20)) (rental-price uint) (genre (string-ascii 30)))
  (let
    (
      (book-id (var-get next-book-id))
      (current-block stacks-block-height)
    )
    (asserts! (> rental-price u0) err-invalid-amount)
    (asserts! (> (len genre) u0) err-invalid-genre)
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
        created-at: current-block,
        genre: genre,
        popularity-score: u0,
        recommendation-count: u0
      }
    )
    (var-set next-book-id (+ book-id u1))
    (var-set total-books (+ (var-get total-books) u1))
    (update-genre-stats genre u1 u0)
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
    (map-set books book-id (merge book {available: false, total-borrows: (+ (get total-borrows book) u1), popularity-score: (+ (get popularity-score book) u1)}))
    (map-set book-borrowers {book-id: book-id, borrower: tx-sender}
      {
        borrowed-at: current-block,
        due-date: due-date,
        returned: false
      }
    )
    (update-genre-stats (get genre book) u0 u1)
    (update-user-genre-preference tx-sender (get genre book))
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

(define-public (create-reading-list (name (string-ascii 50)) (description (string-ascii 200)) (is-public bool))
  (let
    (
      (list-id (var-get next-list-id))
      (current-block stacks-block-height)
    )
    (asserts! (> (len name) u0) err-invalid-amount)
    (map-set reading-lists list-id
      {
        owner: tx-sender,
        name: name,
        description: description,
        created-at: current-block,
        is-public: is-public,
        book-count: u0,
        followers: u0
      }
    )
    (var-set next-list-id (+ list-id u1))
    (var-set total-reading-lists (+ (var-get total-reading-lists) u1))
    (try! (ft-mint? read-token u50 tx-sender))
    (ok list-id)
  )
)

(define-public (add-book-to-list (list-id uint) (book-id uint) (notes (string-ascii 300)))
  (let
    (
      (reading-list (unwrap! (map-get? reading-lists list-id) err-list-not-found))
      (book (unwrap! (map-get? books book-id) err-not-found))
      (current-block stacks-block-height)
      (current-position (get book-count reading-list))
    )
    (asserts! (is-eq tx-sender (get owner reading-list)) err-unauthorized)
    (asserts! (< current-position u50) err-list-full)
    (asserts! (is-none (map-get? reading-list-books {list-id: list-id, book-id: book-id})) err-already-in-list)
    (map-set reading-list-books {list-id: list-id, book-id: book-id}
      {
        added-at: current-block,
        position: current-position,
        notes: notes
      }
    )
    (map-set reading-lists list-id (merge reading-list {book-count: (+ current-position u1)}))
    (try! (ft-mint? read-token u10 tx-sender))
    (ok true)
  )
)

(define-public (remove-book-from-list (list-id uint) (book-id uint))
  (let
    (
      (reading-list (unwrap! (map-get? reading-lists list-id) err-list-not-found))
      (list-book (unwrap! (map-get? reading-list-books {list-id: list-id, book-id: book-id}) err-not-in-list))
    )
    (asserts! (is-eq tx-sender (get owner reading-list)) err-unauthorized)
    (map-delete reading-list-books {list-id: list-id, book-id: book-id})
    (map-set reading-lists list-id (merge reading-list {book-count: (- (get book-count reading-list) u1)}))
    (ok true)
  )
)

(define-public (follow-reading-list (list-id uint))
  (let
    (
      (reading-list (unwrap! (map-get? reading-lists list-id) err-list-not-found))
      (current-block stacks-block-height)
    )
    (asserts! (get is-public reading-list) err-unauthorized)
    (asserts! (not (is-eq tx-sender (get owner reading-list))) err-unauthorized)
    (asserts! (is-none (map-get? list-followers {list-id: list-id, follower: tx-sender})) err-already-exists)
    (map-set list-followers {list-id: list-id, follower: tx-sender}
      {
        followed-at: current-block,
        notifications: true
      }
    )
    (map-set reading-lists list-id (merge reading-list {followers: (+ (get followers reading-list) u1)}))
    (try! (ft-mint? read-token u25 tx-sender))
    (ok true)
  )
)

(define-public (unfollow-reading-list (list-id uint))
  (let
    (
      (reading-list (unwrap! (map-get? reading-lists list-id) err-list-not-found))
      (follow-info (unwrap! (map-get? list-followers {list-id: list-id, follower: tx-sender}) err-not-found))
    )
    (map-delete list-followers {list-id: list-id, follower: tx-sender})
    (map-set reading-lists list-id (merge reading-list {followers: (- (get followers reading-list) u1)}))
    (ok true)
  )
)

(define-public (recommend-book (book-id uint) (recommended-to principal) (reason (string-ascii 200)))
  (let
    (
      (book (unwrap! (map-get? books book-id) err-not-found))
      (current-block stacks-block-height)
    )
    (asserts! (not (is-eq tx-sender recommended-to)) err-unauthorized)
    (asserts! (> (len reason) u0) err-invalid-amount)
    (map-set book-recommendations {recommender: tx-sender, book-id: book-id, recommended-to: recommended-to}
      {
        created-at: current-block,
        reason: reason,
        accepted: false
      }
    )
    (map-set books book-id (merge book {recommendation-count: (+ (get recommendation-count book) u1)}))
    (try! (ft-mint? read-token u15 tx-sender))
    (ok true)
  )
)

(define-public (accept-recommendation (recommender principal) (book-id uint))
  (let
    (
      (recommendation (unwrap! (map-get? book-recommendations {recommender: recommender, book-id: book-id, recommended-to: tx-sender}) err-not-found))
    )
    (asserts! (not (get accepted recommendation)) err-already-exists)
    (map-set book-recommendations {recommender: recommender, book-id: book-id, recommended-to: tx-sender}
      (merge recommendation {accepted: true})
    )
    (try! (ft-mint? read-token u30 tx-sender))
    (try! (ft-mint? read-token u20 recommender))
    (ok true)
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

(define-read-only (get-reading-list (list-id uint))
  (map-get? reading-lists list-id)
)

(define-read-only (get-reading-list-book (list-id uint) (book-id uint))
  (map-get? reading-list-books {list-id: list-id, book-id: book-id})
)

(define-read-only (get-genre-stats (genre (string-ascii 30)))
  (default-to
    {total-books: u0, total-borrows: u0, avg-rating: u0, trending-score: u0}
    (map-get? genre-stats genre)
  )
)

(define-read-only (get-user-genre-preference (user principal) (genre (string-ascii 30)))
  (default-to
    {books-read: u0, last-activity: u0, preference-score: u0}
    (map-get? user-genre-preferences {user: user, genre: genre})
  )
)

(define-read-only (get-book-recommendation (recommender principal) (book-id uint) (recommended-to principal))
  (map-get? book-recommendations {recommender: recommender, book-id: book-id, recommended-to: recommended-to})
)

(define-read-only (get-list-follower-info (list-id uint) (follower principal))
  (map-get? list-followers {list-id: list-id, follower: follower})
)

(define-read-only (get-total-reading-lists)
  (var-get total-reading-lists)
)

(define-read-only (get-next-list-id)
  (var-get next-list-id)
)

(define-read-only (is-following-list (list-id uint) (user principal))
  (is-some (map-get? list-followers {list-id: list-id, follower: user}))
)

(define-read-only (get-book-popularity (book-id uint))
  (match (map-get? books book-id)
    book (get popularity-score book)
    u0
  )
)

(define-read-only (get-book-recommendations-count (book-id uint))
  (match (map-get? books book-id)
    book (get recommendation-count book)
    u0
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

(define-private (update-genre-stats (genre (string-ascii 30)) (books-delta uint) (borrows-delta uint))
  (let
    (
      (current-stats (default-to {total-books: u0, total-borrows: u0, avg-rating: u0, trending-score: u0} (map-get? genre-stats genre)))
      (new-total-books (+ (get total-books current-stats) books-delta))
      (new-total-borrows (+ (get total-borrows current-stats) borrows-delta))
      (trending-score (+ (* new-total-borrows u2) new-total-books))
    )
    (map-set genre-stats genre
      {
        total-books: new-total-books,
        total-borrows: new-total-borrows,
        avg-rating: (get avg-rating current-stats),
        trending-score: trending-score
      }
    )
  )
)

(define-private (update-user-genre-preference (user principal) (genre (string-ascii 30)))
  (let
    (
      (current-pref (default-to {books-read: u0, last-activity: u0, preference-score: u0} (map-get? user-genre-preferences {user: user, genre: genre})))
      (current-block stacks-block-height)
      (new-books-read (+ (get books-read current-pref) u1))
      (new-preference-score (+ (get preference-score current-pref) u10))
    )
    (map-set user-genre-preferences {user: user, genre: genre}
      {
        books-read: new-books-read,
        last-activity: current-block,
        preference-score: new-preference-score
      }
    )
  )
)