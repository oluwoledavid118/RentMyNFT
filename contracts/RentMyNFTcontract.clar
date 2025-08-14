
;; title: RentMyNFT
;; version: 1.0.0
;; summary: A decentralized NFT rental marketplace enabling time-based NFT leasing
;; description: This contract allows NFT owners to list their tokens for rent with customizable
;;              pricing and duration. Renters can lease NFTs with automatic returns after the
;;              rental period expires, backed by a collateral system and revenue sharing model.

;; traits
(define-trait nft-trait
  (
    (get-last-token-id () (response uint uint))
    (get-token-uri (uint) (response (optional (string-ascii 256)) uint))
    (get-owner (uint) (response (optional principal) uint))
    (transfer (uint principal principal) (response bool uint))
  )
)

;; token definitions
;; Using STX as the base currency for rentals and collateral

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u401))
(define-constant ERR_NOT_FOUND (err u404))
(define-constant ERR_ALREADY_EXISTS (err u409))
(define-constant ERR_INVALID_AMOUNT (err u400))
(define-constant ERR_INVALID_DURATION (err u400))
(define-constant ERR_RENTAL_ACTIVE (err u403))
(define-constant ERR_RENTAL_EXPIRED (err u410))
(define-constant ERR_INSUFFICIENT_COLLATERAL (err u402))
(define-constant ERR_TRANSFER_FAILED (err u500))
(define-constant ERR_INVALID_NFT (err u400))

(define-constant PLATFORM_FEE_BASIS_POINTS u250) ;; 2.5%
(define-constant BASIS_POINTS u10000)
(define-constant MIN_RENTAL_DURATION u144) ;; ~24 hours in blocks (10min blocks)
(define-constant MAX_RENTAL_DURATION u4320) ;; ~30 days in blocks

;; data vars
(define-data-var next-rental-id uint u1)
(define-data-var platform-treasury principal CONTRACT_OWNER)

;; data maps
(define-map rental-listings
  { rental-id: uint }
  {
    nft-contract: principal,
    token-id: uint,
    owner: principal,
    price-per-block: uint,
    min-duration: uint,
    max-duration: uint,
    collateral-required: uint,
    is-active: bool,
    created-at: uint
  }
)

(define-map active-rentals
  { rental-id: uint }
  {
    renter: principal,
    start-block: uint,
    end-block: uint,
    total-cost: uint,
    collateral-amount: uint,
    returned: bool
  }
)

(define-map nft-to-rental
  { nft-contract: principal, token-id: uint }
  { rental-id: uint }
)

(define-map user-rental-count
  { user: principal }
  { count: uint }
)

(define-map platform-earnings
  { token: principal }
  { amount: uint }
)

;; public functions

;; List an NFT for rent
(define-public (list-nft-for-rent
  (nft-contract <nft-trait>)
  (token-id uint)
  (price-per-block uint)
  (min-duration uint)
  (max-duration uint)
  (collateral-required uint)
)
  (let (
    (rental-id (var-get next-rental-id))
    (nft-contract-principal (contract-of nft-contract))
  )
    ;; Validate inputs
    (asserts! (> price-per-block u0) ERR_INVALID_AMOUNT)
    (asserts! (>= min-duration MIN_RENTAL_DURATION) ERR_INVALID_DURATION)
    (asserts! (<= max-duration MAX_RENTAL_DURATION) ERR_INVALID_DURATION)
    (asserts! (<= min-duration max-duration) ERR_INVALID_DURATION)
    (asserts! (>= collateral-required u0) ERR_INVALID_AMOUNT)
    
    ;; Check NFT ownership
    (match (contract-call? nft-contract get-owner token-id)
      success (asserts! (is-eq (some tx-sender) success) ERR_NOT_AUTHORIZED)
      error (err error)
    )
    
    ;; Ensure NFT is not already listed
    (asserts! (is-none (map-get? nft-to-rental { nft-contract: nft-contract-principal, token-id: token-id })) ERR_ALREADY_EXISTS)
    
    ;; Create rental listing
    (map-set rental-listings
      { rental-id: rental-id }
      {
        nft-contract: nft-contract-principal,
        token-id: token-id,
        owner: tx-sender,
        price-per-block: price-per-block,
        min-duration: min-duration,
        max-duration: max-duration,
        collateral-required: collateral-required,
        is-active: true,
        created-at: block-height
      }
    )
    
    ;; Map NFT to rental ID
    (map-set nft-to-rental
      { nft-contract: nft-contract-principal, token-id: token-id }
      { rental-id: rental-id }
    )
    
    ;; Increment rental ID counter
    (var-set next-rental-id (+ rental-id u1))
    
    (print { event: "nft-listed", rental-id: rental-id, nft-contract: nft-contract-principal, token-id: token-id, owner: tx-sender })
    (ok rental-id)
  )
)

;; Rent an NFT
(define-public (rent-nft
  (rental-id uint)
  (duration uint)
  (nft-contract <nft-trait>)
)
  (let (
    (listing (unwrap! (map-get? rental-listings { rental-id: rental-id }) ERR_NOT_FOUND))
    (total-cost (* (get price-per-block listing) duration))
    (platform-fee (/ (* total-cost PLATFORM_FEE_BASIS_POINTS) BASIS_POINTS))
    (owner-payment (- total-cost platform-fee))
    (collateral-required (get collateral-required listing))
    (end-block (+ block-height duration))
  )
    ;; Validate rental
    (asserts! (get is-active listing) ERR_NOT_FOUND)
    (asserts! (>= duration (get min-duration listing)) ERR_INVALID_DURATION)
    (asserts! (<= duration (get max-duration listing)) ERR_INVALID_DURATION)
    (asserts! (is-none (map-get? active-rentals { rental-id: rental-id })) ERR_RENTAL_ACTIVE)
    (asserts! (is-eq (get nft-contract listing) (contract-of nft-contract)) ERR_INVALID_NFT)
    
    ;; Transfer rental payment to owner
    (try! (stx-transfer? owner-payment tx-sender (get owner listing)))
    
    ;; Transfer platform fee
    (try! (stx-transfer? platform-fee tx-sender (var-get platform-treasury)))
    
    ;; Handle collateral if required
    (if (> collateral-required u0)
      (try! (stx-transfer? collateral-required tx-sender (as-contract tx-sender)))
      true
    )
    
    ;; Transfer NFT to renter
    (match (as-contract (contract-call? nft-contract transfer (get token-id listing) (get owner listing) tx-sender))
      success (asserts! success ERR_TRANSFER_FAILED)
      error (err error)
    )
    
    ;; Create active rental record
    (map-set active-rentals
      { rental-id: rental-id }
      {
        renter: tx-sender,
        start-block: block-height,
        end-block: end-block,
        total-cost: total-cost,
        collateral-amount: collateral-required,
        returned: false
      }
    )
    
    ;; Update user rental count
    (map-set user-rental-count
      { user: tx-sender }
      { count: (+ (default-to u0 (get count (map-get? user-rental-count { user: tx-sender }))) u1) }
    )
    
    ;; Update platform earnings
    (map-set platform-earnings
      { token: .stx }
      { amount: (+ (default-to u0 (get amount (map-get? platform-earnings { token: .stx }))) platform-fee) }
    )
    
    (print { event: "nft-rented", rental-id: rental-id, renter: tx-sender, duration: duration, total-cost: total-cost })
    (ok true)
  )
)

;; Return NFT after rental period
(define-public (return-nft
  (rental-id uint)
  (nft-contract <nft-trait>)
)
  (let (
    (listing (unwrap! (map-get? rental-listings { rental-id: rental-id }) ERR_NOT_FOUND))
    (rental (unwrap! (map-get? active-rentals { rental-id: rental-id }) ERR_NOT_FOUND))
  )
    ;; Validate return conditions
    (asserts! (not (get returned rental)) ERR_NOT_FOUND)
    (asserts! (>= block-height (get end-block rental)) ERR_RENTAL_ACTIVE)
    (asserts! (is-eq (get nft-contract listing) (contract-of nft-contract)) ERR_INVALID_NFT)
    
    ;; Transfer NFT back to owner
    (match (as-contract (contract-call? nft-contract transfer (get token-id listing) (get renter rental) (get owner listing)))
      success (asserts! success ERR_TRANSFER_FAILED)
      error (err error)
    )
    
    ;; Return collateral to renter if any
    (if (> (get collateral-amount rental) u0)
      (try! (as-contract (stx-transfer? (get collateral-amount rental) tx-sender (get renter rental))))
      true
    )
    
    ;; Mark rental as returned
    (map-set active-rentals
      { rental-id: rental-id }
      (merge rental { returned: true })
    )
    
    (print { event: "nft-returned", rental-id: rental-id, renter: (get renter rental) })
    (ok true)
  )
)

;; Cancel rental listing
(define-public (cancel-listing (rental-id uint))
  (let (
    (listing (unwrap! (map-get? rental-listings { rental-id: rental-id }) ERR_NOT_FOUND))
  )
    ;; Only owner can cancel
    (asserts! (is-eq tx-sender (get owner listing)) ERR_NOT_AUTHORIZED)
    ;; Cannot cancel if actively rented
    (asserts! (is-none (map-get? active-rentals { rental-id: rental-id })) ERR_RENTAL_ACTIVE)
    
    ;; Deactivate listing
    (map-set rental-listings
      { rental-id: rental-id }
      (merge listing { is-active: false })
    )
    
    ;; Remove NFT mapping
    (map-delete nft-to-rental { nft-contract: (get nft-contract listing), token-id: (get token-id listing) })
    
    (print { event: "listing-cancelled", rental-id: rental-id })
    (ok true)
  )
)

;; Update platform treasury (admin only)
(define-public (set-platform-treasury (new-treasury principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set platform-treasury new-treasury)
    (ok true)
  )
)


;; read only functions

;; Get rental listing details
(define-read-only (get-rental-listing (rental-id uint))
  (map-get? rental-listings { rental-id: rental-id })
)

;; Get active rental details
(define-read-only (get-active-rental (rental-id uint))
  (map-get? active-rentals { rental-id: rental-id })
)

;; Get rental ID for NFT
(define-read-only (get-nft-rental-id (nft-contract principal) (token-id uint))
  (map-get? nft-to-rental { nft-contract: nft-contract, token-id: token-id })
)

;; Check if rental has expired
(define-read-only (is-rental-expired (rental-id uint))
  (match (map-get? active-rentals { rental-id: rental-id })
    rental (>= block-height (get end-block rental))
    false
  )
)

;; Get user rental count
(define-read-only (get-user-rental-count (user principal))
  (default-to u0 (get count (map-get? user-rental-count { user: user })))
)

;; Get platform earnings
(define-read-only (get-platform-earnings (token principal))
  (default-to u0 (get amount (map-get? platform-earnings { token: token })))
)

;; Get next rental ID
(define-read-only (get-next-rental-id)
  (var-get next-rental-id)
)

;; Get platform treasury
(define-read-only (get-platform-treasury)
  (var-get platform-treasury)
)

;; Calculate rental cost
(define-read-only (calculate-rental-cost (rental-id uint) (duration uint))
  (match (map-get? rental-listings { rental-id: rental-id })
    listing 
    (let (
      (total-cost (* (get price-per-block listing) duration))
      (platform-fee (/ (* total-cost PLATFORM_FEE_BASIS_POINTS) BASIS_POINTS))
    )
      (some {
        total-cost: total-cost,
        platform-fee: platform-fee,
        owner-payment: (- total-cost platform-fee),
        collateral-required: (get collateral-required listing)
      })
    )
    none
  )
)