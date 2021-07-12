;; MARKET-CONTRACT
(impl-trait .marketplace-trait.marketplace-trait)
(use-trait market-genuine-trait .marketplace-trait.market-genuine-trait)

;; This trait is a subset of the functions of sip-009 trait for NFTs.
;; (define-trait my-market-trait
;;   (
;;     (get-pending-licensing (uint principal principal) (response bool uint))
;;   )
;; )

;; (define-trait market-genuine-trait
;;   (
;;      ;; Owner of a given token identifier
;;     (get-owner (uint) (response (optional principal) uint))
;;     ;; Transfer from the sender to a new principal
;;     (transfer (uint principal principal) (response bool uint))

;;     (attach-license (uint principal principal (principal <my-market-trait>)) (response bool bool))

;;   )
;; )


(define-constant genuine-contract .genuine)

(define-map on-sale
  {owner: principal, tradables: principal, tradable-id: uint}
  {price: uint, duration: uint}
)

(define-map pending-sale {tradables: principal, tradable-id: uint}
  {owner: principal, new-owner: principal, price: uint, public-key: (string-ascii 66)}
)

(define-map pending-licensing {tradables: principal, tradable-id: uint, new-owner: principal}
  {license-type: uint, duration: uint}
)


(define-read-only (get-pending-licensing (tradeable-id uint) (tradeable principal) (new-owner principal))
  (ok true)
  ;; (let 
  ;;   (
  ;;     (lincensing (unwrap! (map-get? pending-licensing {tradables: (contract-of tradeable), tradable-id: tradeable-id, new-owner: new-owner}) (err err-tradable-not-on-sale)))
  ;;   )
  ;;   (ok lincensing)
  ;; )
)


(define-constant err-sender-insufficient-balance u1)
(define-constant err-same-sender-and-recipient-principal u2)
(define-constant err-stx-amount-non-positive u3)
(define-constant err-invalid-sender-principal u4)

(define-constant err-invalid-offer-key u5)
(define-constant err-payment-failed u6)
(define-constant err-transfer-failed u7)
(define-constant err-not-allowed u8)
(define-constant err-duplicate-entry u9)
(define-constant err-tradable-not-found u10)
(define-constant err-tradable-not-on-sale u11)
(define-constant err-tradable-not-on-pending-sale u12)
(define-constant err-wrong-payment-amount u13)
(define-constant err-invalid-owner u13)
(define-constant err-tradable-transfer-failed u14)

(define-private (transfer-to-escrow (tradables <market-genuine-trait>) (tradable-id uint) (payment uint) (tradable-owner principal) (public-key (string-ascii 66)))
    (match (stx-transfer? payment tx-sender (as-contract tx-sender))
      stx-transfer-success 
        (match 
          (contract-call? tradables transfer tradable-id tradable-owner (as-contract tx-sender))
            trandable-transfer-success (begin 
              (map-delete on-sale {owner: tradable-owner, tradables: (contract-of tradables), tradable-id: tradable-id})
              (ok (map-insert pending-sale {tradables: (contract-of tradables), tradable-id: tradable-id} {owner: tradable-owner, new-owner: tx-sender, price: payment, public-key: public-key}))
            )
            trandable-transfer-error (err trandable-transfer-error)
        )
      stx-transfer-error (err stx-transfer-error)
    )
)



(define-private (get-owner (tradables <market-genuine-trait>) (tradable-id uint))
  (contract-call? tradables get-owner tradable-id)
)

;; called by the owner
(define-public (offer-tradable (tradables <market-genuine-trait>) (tradable-id uint) (price uint) (duration uint))
  (let ((tradable-owner (unwrap! (unwrap-panic (get-owner tradables tradable-id)) (err err-tradable-not-found))))
    (if (is-eq tradable-owner tx-sender)
      (if (map-insert on-sale {owner: tradable-owner, tradables: (contract-of tradables), tradable-id: tradable-id}
                {price: price, duration: duration})
          (ok true)
          (err err-duplicate-entry)
      )
      (err err-not-allowed)
    )
  )
)


;; called by the bidder ;-)
(define-public (pay (tradables <market-genuine-trait>) (tradable-id uint) (payment uint) (public-key (string-ascii 66)))
  (let (
      (tradable-owner (unwrap! (unwrap-panic (get-owner tradables tradable-id)) (err err-tradable-not-found)))
      (price (get price (unwrap! (map-get? on-sale {owner: tradable-owner, tradables: (contract-of tradables), tradable-id: tradable-id}) (err err-tradable-not-on-sale))))
    )
    (if (is-eq price payment)
      (begin 
        (transfer-to-escrow tradables tradable-id payment tradable-owner public-key)
      )
      (err err-wrong-payment-amount)
    )
  )
)

;; (define-map pending-sale {tradables: principal, tradable-id: uint}
;;   {owner: principal, new-owner: principal, public-key: (string-ascii 66)}
;; )

;; called by the bidder
(define-public (transfer (tradables <market-genuine-trait>) (tradable-id uint))
  (let ((contract (contract-of tradables)))
    (let 
      (
        (sale (unwrap! (map-get? pending-sale {tradables: (contract-of tradables), tradable-id: tradable-id}) (err err-tradable-not-on-pending-sale)))
      )
      (if (is-eq (get owner sale) tx-sender)
        (match (as-contract (stx-transfer? (get price sale) tx-sender (get owner sale)))
          success (match (as-contract (contract-call? tradables transfer tradable-id tx-sender (get new-owner sale)))
              transferred (begin
                (ok true)
              )
              error (err err-transfer-failed)
          )
          error (err err-payment-failed)
        )
        (err err-invalid-owner)
      )
    )
  )
)


(define-public (test (tradables <market-genuine-trait>) (tradeable-id uint) (license-type uint) (duration uint) (new-owner principal))
  (ok true)
  ;; (if (is-eq (contract-of tradables) genuine-contract)
  ;;   (ok true)
  ;;     ;; (if (map-insert pending-licensing {tradables: (contract-of tradables), tradable-id: tradeable-id, new-owner: new-owner}
  ;;     ;;             {license-type: license-type, duration: duration})
  ;;     ;;       (
  ;;     ;;         match (contract-call? tradables attach-license tradeable-id new-owner (contract-of tradables) (contract-of market))
  ;;     ;;           success (ok true)
  ;;     ;;           error (err u1)
              
  ;;     ;;       )
  ;;     ;;       (err err-duplicate-entry)
  ;;     ;; )
  ;;   (err u1)
  ;; )
)