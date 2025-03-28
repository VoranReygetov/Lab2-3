(load "MPZ.lisp")

;; 1. Define main frames for the system

(DEFRAME 'CLIENT
  '(TYPE ($DEFAULT "Seller") ($IF-NEEDED (lambda () (if (FGETV self 'TREATIES) "Seller" "Buyer")))))
(DEFRAME 'TREATY
  '(BONUS ($IF-NEEDED (lambda () (* 0.05 (FGETV self 'COST))))))

(DEFRAME 'FLAT
  '(RATE ($DEFAULT 0.05) 
         ($IF-NEEDED 
          (lambda () 
            (cond 
              ((equal (FGETV self 'DISTRICT) "Center") 0.07)
              ((equal (FGETV self 'DISTRICT) "Sleeping") 0.05)
              (t 0.04))))))

;; 2. Utility functions with proper frame handling
(defun ADD-CLIENT (name address phone)
  "Creates a new client frame with generated ID and basic information.
  Returns the generated client ID."
  (let ((id (FGENNAME)))
    (FASSERT id
             `(REGISTR ($VALUE ,id))
             `(NAME ($VALUE ,name))
             `(ADDRESS ($VALUE ,address))
             `(PHONE ($VALUE ,phone))
             `(AKO ($VALUE CLIENT)))  
    id))

(defun ADD-FLAT (address district floor floors type-house type-plan type-toilet sq-all sq-life sq-kit privat phone document)
  "Creates a new flat frame with detailed property information.
  Returns the generated flat ID."
  (let ((id (FGENNAME)))
    (FASSERT id  
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
             `(DOCUMENT ($VALUE ,document))
             `(AKO ($VALUE FLAT))) 
    id))

(defun CREATE-TREATY (client-id flat-id date-start date-end cost)
  "Creates a new treaty between client and flat with specified terms.
  Updates client's treaty list and returns the generated treaty ID."
  (let ((treaty-id (FGENNAME)))
    ;; Verify frames exist
    (unless (assoc client-id *FRAMES*)
      (error "Client with ID ~A not found" client-id))
    (unless (assoc flat-id *FRAMES*)
      (error "Flat with ID ~A not found" flat-id))

    ;; Add treaty
    (FASSERT treaty-id
             `(ID ($VALUE ,treaty-id))
             `(CLIENT ($VALUE ,client-id))
             `(FLAT ($VALUE ,flat-id))
             `(DATE-START ($VALUE ,date-start))
             `(DATE-END ($VALUE ,date-end))
             `(COST ($VALUE ,cost))
             `(AKO ($VALUE TREATY)))  

    ;; Update client's treaties list
    (let ((current-treaties (FGETV client-id 'TREATIES)))
      (FPUT client-id 'TREATIES '$VALUE (cons treaty-id (or current-treaties '()))))

    treaty-id))

(defun UPDATE-COST (treaty-id new-cost)
  "Updates the cost value of a specified treaty.
  Replaces the previous cost value completely."
  (unless (assoc treaty-id *FRAMES*)
    (error "Treaty with ID ~A not found" treaty-id))
  (FPUT treaty-id 'COST '$VALUE new-cost))

(defun EXTEND-TREATY (treaty-id new-end-date)
  "Extends the end date of a specified treaty.
  Replaces the previous end date completely."
  (unless (assoc treaty-id *FRAMES*)
    (error "Treaty with ID ~A not found" treaty-id))
  (FPUT treaty-id 'DATE-END '$VALUE new-end-date))

(defun FIND-FLATS-BY-PARAMS (district min-floor max-floor type-house min-sq max-sq)
  "Searches for flats matching specified criteria:
  - District name
  - Floor range (min-max)
  - House type
  - Square footage range (min-max)
  Returns list of matching flat IDs."
  (let ((result nil))
    (dolist (frame-pair *FRAMES* result)
      (let* ((frame-name (car frame-pair))
             (frame (cdr frame-pair)))
        (when (and (equal (first (FGETV frame-name 'AKO)) 'FLAT)
                   (equal (first (FGETV frame-name 'DISTRICT)) district)
                   (>= (first (FGETV frame-name 'AT-FLOOR)) min-floor)
                   (<= (first (FGETV frame-name 'AT-FLOOR)) max-floor)
                   (equal (first (FGETV frame-name 'TYPE-HOUSE)) type-house)
                   (>= (first (FGETV frame-name 'SQ-ALL)) min-sq)
                   (<= (first (FGETV frame-name 'SQ-ALL)) max-sq))
          (push frame-name result))))))