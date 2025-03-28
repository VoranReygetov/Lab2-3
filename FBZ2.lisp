(load "MPZ.lisp")

;; 1. Define main frames for the system

;; Client frame
(DEFRAME 'CLIENT
  '(REGISTR ($VALUE))
  '(ADDRESS ($VALUE))
  '(NAME ($VALUE))
  '(PHONE ($VALUE))
  '(TREATIES ($VALUE))  ;; List of treaty IDs
  '(TYPE ($DEFAULT "Seller") ($IF-NEEDED (lambda () (if (FGETV self 'TREATIES) "Seller" "Buyer"))))
)
;; Treaty frame
(DEFRAME 'TREATY
  '(ID ($VALUE))
  '(CLIENT ($VALUE))    ;; Client ID
  '(FLAT ($VALUE))     ;; Flat ID
  '(DATE-START ($VALUE))
  '(DATE-END ($VALUE))
  '(COST ($VALUE))
  '(BONUS ($VALUE ($IF-NEEDED (lambda () (* 0.05 (FGETV self 'COST))))))
)
;; Flat frame
(DEFRAME 'FLAT
  '(ADDRESS ($VALUE))
  '(DISTRICT ($VALUE))
  '(FLOORS ($VALUE))
  '(AT-FLOOR ($VALUE))
  '(TYPE-HOUSE ($VALUE))
  '(TYPE-PLAN ($VALUE))
  '(TYPE-TOILET ($VALUE))
  '(SQ-ALL ($VALUE))
  '(SQ-LIFE ($VALUE))
  '(SQ-KIT ($VALUE))
  '(PRIVAT ($VALUE))
  '(SIGN-PHONE ($VALUE))
  '(DOCUMENT ($VALUE))
  '(RATE ($VALUE ($DEFAULT 0.05) ($IF-NEEDED (lambda () (cond 
                                                       ((equal (FGETV self 'DISTRICT) "Center") 0.07)
                                                       ((equal (FGETV self 'DISTRICT) "Sleeping") 0.05)
                                                       (t 0.04))))))
)
;; 2. Utility functions with proper frame handling

(defun ADD-CLIENT (name address phone)
  (let ((id (FGENNAME)))
    (FASSERT 'CLIENT 
             `(REGISTR ($VALUE ,id))
             `(NAME ($VALUE ,name))
             `(ADDRESS ($VALUE ,address))
             `(PHONE ($VALUE ,phone)))
    id))

(defun ADD-FLAT (address district floor floors type-house type-plan type-toilet sq-all sq-life sq-kit privat phone document)
  (let ((id (FGENNAME)))
    (FASSERT 'FLAT
             `(ADDRESS ($VALUE ,address))
             `(DISTRICT ($VALUE ,district))
             `(AT-FLOOR ($VALUE ,floor))
             `(FLOORS ($VALUE ,floors))
             `(TYPE-HOUSE ($VALUE ,type-house))
             `(TYPE-PLAN ($VALUE ,type-plan))
             `(TYPE-TOILET ($VALUE ,type-toilet))
             `(SQ-ALL ($VALUE ,sq-all))
             `(SQ-LIFE ($VALUE ,sq-life))
             `(SQ-KIT ($VALUE ,sq-kit))
             `(PRIVAT ($VALUE ,privat))
             `(SIGN-PHONE ($VALUE ,phone))
             `(DOCUMENT ($VALUE ,document)))
    id))

(defun CREATE-TREATY (client-id flat-id date-start date-end cost)
  (let ((treaty-id (FGENNAME)))
    ;; Verify frames exist
    (unless (FGET 'CLIENT client-id)
      (error "Client with ID ~A not found" client-id))
    (unless (FGET 'FLAT flat-id)
      (error "Flat with ID ~A not found" flat-id))

    ;; Add treaty
    (FASSERT 'TREATY
             `(ID ($VALUE ,treaty-id))
             `(CLIENT ($VALUE ,client-id))
             `(FLAT ($VALUE ,flat-id))
             `(DATE-START ($VALUE ,date-start))
             `(DATE-END ($VALUE ,date-end))
             `(COST ($VALUE ,cost)))

    ;; Update client's treaties list
    (let ((current-treaties (FGETV client-id 'TREATIES)))
      (FPUT client-id 'TREATIES '$VALUE (cons treaty-id (or current-treaties '()))))

    treaty-id))

; (defun TEST-SYSTEM ()
;   (handler-case
;       (progn
;         ;; Add client
;         (let ((client1 (ADD-CLIENT "John Doe" "Main St, 10" "0991234567")))
;           ;; Add flat
;           (let ((flat1 (ADD-FLAT
;                           "Main St, 10, #5" "Center" 3 9 "brick" "new" "separate"
;                           65 42 10 t t "Purchase agreement")))
;             ;; Create treaty
;             (let ((treaty1 (CREATE-TREATY client1 flat1 "2023-01-15" "2023-07-15" 500000)))
;               ;; Update cost
;               (UPDATE-COST treaty1 550000)
;               ;; Extend treaty
;               (EXTEND-TREATY treaty1 "2023-10-15")
;               ;; Find flats
;               (format t "Search results: ~a~%" (FIND-FLATS-BY-PARAMS "Center" 1 5 "brick" 50 80))))))
;       (error (e) (format t "Error: ~a~%" e))))

; ;; Run the test
; (TEST-SYSTEM)