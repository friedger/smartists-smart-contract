(define-trait marketplace-trait
  (
    ;; (get-pending-licensing (uint principal principal) (response (tuple (license-type uint) (duration uint)) uint))
    (get-pending-licensing (uint principal principal) (response bool bool))
  )
)


(define-trait market-genuine-trait
  (
     ;; Owner of a given token identifier
    (get-owner (uint) (response (optional principal) uint))
    ;; Transfer from the sender to a new principal
    (transfer (uint principal principal) (response bool uint))

    (attach-license (uint principal principal <marketplace-trait>) (response bool bool))

  )
)