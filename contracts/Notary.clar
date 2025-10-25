(define-constant ERR-RECORD-EXISTS (err u1))
(define-constant ERR-RECORD-NOT-FOUND (err u2))
(define-constant ERR-INVALID-HASH (err u3))
(define-constant ERR-UNAUTHORIZED (err u4))

(define-data-var total-records uint u0)
(define-data-var contract-owner principal tx-sender)

(define-map records
  { record-hash: (buff 32) }
  {
    submitter: principal,
    timestamp: uint,
    notarized: bool,
    metadata: (string-ascii 256)
  }
)

(define-map record-list
  { record-index: uint }
  { record-hash: (buff 32) }
)

(define-public (notarize-record (hash (buff 32)) (metadata (string-ascii 256)))
  (let
    (
      (existing (map-get? records { record-hash: hash }))
      (new-index (var-get total-records))
    )
    (if (is-some existing)
      ERR-RECORD-EXISTS
      (begin
        (map-set records
          { record-hash: hash }
          {
            submitter: tx-sender,
            timestamp: burn-block-height,
            notarized: true,
            metadata: metadata
          }
        )
        (map-set record-list
          { record-index: new-index }
          { record-hash: hash }
        )
        (var-set total-records (+ new-index u1))
        (ok true)
      )
    )
  )
)

(define-read-only (get-record-info (hash (buff 32)))
  (let
    (
      (record (map-get? records { record-hash: hash }))
    )
    (if (is-some record)
      (ok record)
      ERR-RECORD-NOT-FOUND
    )
  )
)

(define-read-only (verify-record (hash (buff 32)))
  (let
    (
      (record (map-get? records { record-hash: hash }))
    )
    (match record
      record-data (ok (get notarized record-data))
      ERR-RECORD-NOT-FOUND
    )
  )
)

(define-read-only (get-notarization-count)
  (ok (var-get total-records))
)

(define-read-only (get-record-by-index (index uint))
  (let
    (
      (hash-entry (map-get? record-list { record-index: index }))
    )
    (match hash-entry
      entry (ok (get record-hash entry))
      ERR-RECORD-NOT-FOUND
    )
  )
)

(define-public (update-record-metadata (hash (buff 32)) (new-metadata (string-ascii 256)))
  (let
    (
      (record (map-get? records { record-hash: hash }))
    )
    (match record
      record-data
      (if (is-eq tx-sender (get submitter record-data))
        (begin
          (map-set records
            { record-hash: hash }
            (merge record-data { metadata: new-metadata })
          )
          (ok true)
        )
        ERR-UNAUTHORIZED
      )
      ERR-RECORD-NOT-FOUND
    )
  )
)

(define-read-only (get-submitter (hash (buff 32)))
  (let
    (
      (record (map-get? records { record-hash: hash }))
    )
    (match record
      record-data (ok (get submitter record-data))
      ERR-RECORD-NOT-FOUND
    )
  )
)

(define-read-only (get-timestamp (hash (buff 32)))
  (let
    (
      (record (map-get? records { record-hash: hash }))
    )
    (match record
      record-data (ok (get timestamp record-data))
      ERR-RECORD-NOT-FOUND
    )
  )
)

