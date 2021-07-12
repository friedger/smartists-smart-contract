;; GENUINE-CONTRACT
(impl-trait 'ST16KQ2VQSSJFGQJPNYC04P1SGVP77C760AJH38F2.genuine-trait.genuine-trait)
(use-trait genuine-market-trait 'ST16KQ2VQSSJFGQJPNYC04P1SGVP77C760AJH38F2.genuine-trait.genuine-market-trait)

;; (define-trait nft-trait
;;   (
;;     ;; Last token ID, limited to uint range
;;     (get-last-token-id () (response uint uint))

;;     ;; URI for metadata associated with the token
;;     (get-token-uri (uint) (response (optional (string-ascii 256)) uint))

;;      ;; Owner of a given token identifier
;;     (get-owner (uint) (response (optional principal) uint))

;;     ;; Transfer from the sender to a new principal
;;     (transfer (uint principal principal) (response bool uint))

;;     (attach-license (uint principal principal <my-market-trait>) (response bool bool))
;;   )
;; )

;; (define-trait market-trait
;;   (
;;     (get-pending-licensing (uint principal principal) (response (tuple (license-type uint) (duration uint)) uint))
;;   )
;; )


(define-map genuine {genuine-id: uint}
  {name: (string-ascii 20),
  mime-type: (string-ascii 129),
  preview-file-uri: (string-ascii 2048),
  preview-file-filename: (string-ascii 36),
  preview-file-hash: (string-ascii 64),
  main-file-filename: (string-ascii 36),
  main-file-hash: (string-ascii 64),
  timestamp: uint,
  creatorAddress: principal
  }
)

(define-map license
  {genuine-id: uint, owner: principal}
  {licenseType: uint, duration: uint}
)

{name: "text", image: "number-index", timestamp: "block-height"}
(define-read-only (get-meta-data (genuine-id uint))
    (nft-meta-data genuine-id)
)


(define-non-fungible-token nft-genuine uint)
(define-data-var next-id uint u1)

(define-private (get-time)
   (unwrap-panic (get-block-info? time (- block-height u1))))

(define-private (nft-meta-data (genuine-id uint))
    (map-get? genuine {genuine-id: genuine-id})
)

(define-private (add-license (genuine-id uint) (owner principal) (licenseType uint) (duration uint))
    (map-insert license {genuine-id: genuine-id, owner: owner}
                {licenseType: licenseType, duration: duration})
)


{action: "create"}
(define-public (create-genuine (name (string-ascii 20)) (mime-type (string-ascii 129)) (preview-file-uri (string-ascii 2048)) (preview-file-filename (string-ascii 36)) (preview-file-hash (string-ascii 64)) (main-file-filename (string-ascii 36)) (main-file-hash (string-ascii 64)))
    (let ((genuine-id (var-get next-id)))
      (if (is-ok (nft-mint? nft-genuine genuine-id tx-sender))
        (begin
          (var-set next-id (+ genuine-id u1))
          (map-set genuine {genuine-id: genuine-id}
          {
            name: name,
            mime-type: mime-type,
            preview-file-uri: preview-file-uri,
            preview-file-filename: preview-file-filename,
            preview-file-hash: preview-file-hash,
            main-file-filename: main-file-filename,
            main-file-hash: main-file-hash,
            timestamp: (get-time),
            creatorAddress: tx-sender,
          })
          (ok genuine-id))
        err-genuine-exists)))


{action: "transfer"}
(define-public (transfer (genuine-id uint) (sender principal) (recipient principal))
  (let ((owner (unwrap! (unwrap-panic (get-owner genuine-id)) err-genuine-unborn)))
    (if (is-eq owner sender)
      (match (nft-transfer? nft-genuine genuine-id sender recipient)
        success (ok success)
        error (err-nft-transfer error))
      err-transfer-not-allowed)))

{action: "add-license"}
(define-public (attach-license (genuine-id uint) (owner principal) (tradeable principal) (market <genuine-market-trait>))
  (ok true)
)

(define-read-only (get-last-token-id)
  (ok (- (var-get next-id) u1))
)

(define-read-only (get-token-uri (genuine-id uint))
  (ok none))

(define-read-only (get-owner (genuine-id uint))
  (match (nft-get-owner? nft-genuine genuine-id)
    owner (ok (some owner))
    (ok none)
  )
)

;; error handling
(define-constant err-transfer-not-allowed (err u401)) ;; unauthorized
(define-constant err-genuine-unborn (err u404)) ;; not found
(define-constant err-sender-equals-recipient (err u405)) ;; method not allowed
(define-constant err-genuine-exists (err u409)) ;; conflict
(define-constant err-genuine-died (err u501)) ;; internal error

(define-map err-strings (response uint uint) (string-ascii 32))
(map-insert err-strings err-transfer-not-allowed "transfer-not-allowed")
(map-insert err-strings err-genuine-unborn "genuine-unborn")
(map-insert err-strings err-sender-equals-recipient "sender-equals-recipient")
(map-insert err-strings err-genuine-exists "genuine-exists")
(map-insert err-strings err-genuine-died "genuine-died")

(define-private (err-nft-transfer (code uint))
  (if (is-eq u1 code)
    err-transfer-not-allowed
    (if (is-eq u2 code)
      err-sender-equals-recipient
      (if (is-eq u3 code)
        err-genuine-unborn
        (err code)))))

(define-private (err-nft-mint (code uint))
  (if (is-eq u1 code)
    err-genuine-exists
    (err code)))

(define-read-only (get-errstr (code uint))
  (unwrap! (map-get? err-strings (err code)) "unknown-error"))