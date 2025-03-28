(load "FBZ.lisp")

(defun TEST-SYSTEM ()
  (handler-case
      (progn
        ;; Clear previous data
        (setf *FRAMES* nil)
        (setf *FGENAMELIST* nil)
        
        ;; Add client
        (let ((client1 (ADD-CLIENT "John Doe" "Main St, 10" "0991234567")))
          (format t "~%Created client with ID: ~a" client1)
          
          ;; Add flat
          (let ((flat1 (ADD-FLAT
                        "Main St, 10, #5" "Center" 3 9 "brick" "new" "separate"
                        65 42 10 t t "Purchase agreement")))
            (format t "~%Created flat with ID: ~a" flat1)
            
            ;; Create treaty
            (let ((treaty1 (CREATE-TREATY client1 flat1 "2023-01-15" "2023-07-15" 500000)))
              (format t "~%Created treaty with ID: ~a" treaty1)
              ;; Display all frames
              (format t "~%~%All frames in system:~%")
              (FPRINT)
              ;; Update cost
              (UPDATE-COST treaty1 550000)
              (format t "~%Updated treaty cost to 550000")

              ;; Extend treaty
              (EXTEND-TREATY treaty1 "2023-10-15")
              (format t "~%Extended treaty to 2023-10-15")
              
              ;; Display treaty frame after changes
              (format t "~%~%Treaty frame after changes:~%")
              (FPRINT (list (assoc treaty1 *FRAMES*)))
              
              ;; Find flats
              (format t "~%~%Search results for flats in Center district (brick, floor 1-5, sq 50-80):~%")
              (let ((found-flats (FIND-FLATS-BY-PARAMS "Center" 1 5 "brick" 50 80)))
                (if found-flats
                    (dolist (flat found-flats)
                      (format t "~%Found matching flat: ~a" flat)
                      (format t "~%Address: ~a" (first (FGETV flat 'ADDRESS)))
                      (format t "~%District: ~a" (first (FGETV flat 'DISTRICT)))
                      (format t "~%Floor: ~a" (first (FGETV flat 'AT-FLOOR)))
                      (format t "~%Type: ~a" (first (FGETV flat 'TYPE-HOUSE)))
                      (format t "~%Square: ~a~%" (first (FGETV flat 'SQ-ALL))))
                    (format t "No flats found matching criteria~%")))))))
      (error (e) (format t "~%Error: ~a~%" e))))

;; Run the test
(format t "~%Starting test...~%")
(TEST-SYSTEM)
(format t "~%Test completed.~%")