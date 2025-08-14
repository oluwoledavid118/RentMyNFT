
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