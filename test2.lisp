(load "MPZ.lisp")
;; Створення фрейму CLIENT
(DEFRAME 'CLIENT
         '(NAME ($VALUE "Іванов Іван"))
         '(AGE ($VALUE 30))
         '(CITY ($VALUE "Київ")))

;; Тест FASSERT
(FASSERT 'CLIENT '(NAME ($VALUE "Петров Петро")))
(print (FGETV 'CLIENT 'NAME)) ; Очікується: ("Петров Петро")

;; Тест FASSERTQ
(FASSERTQ 'CLIENT '(AGE ($VALUE 35)))
(print (FGETV 'CLIENT 'AGE)) ; Очікується: (35)

;; Тест FGETV
(print (FGETV 'CLIENT 'CITY)) ; Очікується: ("Київ")

;; Тест FGET
(print (FGET 'CLIENT 'NAME '$VALUE)) ; Очікується: ("Петров Петро")

;; Тест FPUTV
(FPUTV 'CLIENT 'CITY "Львів")
(print (FGETV 'CLIENT 'CITY)) ; Очікується: ("Львів")

;; Тест FPUT
(FPUT 'CLIENT 'AGE '$VALUE 40)
(print (FGETV 'CLIENT 'AGE)) ; Очікується: (40)

;; Тест FDELETE
(FDELETE 'CLIENT 'NAME '$VALUE "Петров Петро")
(print (FGETV 'CLIENT 'NAME)) ; Очікується: NIL

;; ... (ваш код з функціями фреймів) ...

;; Тест FMATCH

(DEFRAME 'CLIENT2
         '(NAME ($VALUE "Сидоров Сидір"))
         '(AGE ($VALUE 40))
         '(CITY ($VALUE "Львів")))

(DEFRAME 'CLIENT3
         '(NAME ($VALUE "Коваленко Костянтин"))
         '(AGE ($VALUE 35))
         '(CITY ($VALUE "Одеса")))

;; Тест FMATCH
(setf *MATCHED* (FMATCH 'CLIENT '(CITY ($VALUE "Львів"))))
(print (mapcar #'frame-data-name *MATCHED*)) ;; Очікується (CLIENT2)

;; Тест FDISPLAY
(FDISPLAY 'CLIENT)
;; Очікується виведення:
;; Frame: CLIENT
;;  Slot: NAME
;;   Facet: $VALUE, Values: NIL
;;  Slot: AGE
;;   Facet: $VALUE, Values: (40)
;;  Slot: CITY
;;   Facet: $VALUE, Values: ("Львів")

;; Тест FDISPLAY-ALL
(FDISPLAY-ALL)
;; Очікується виведення даних для всіх фреймів