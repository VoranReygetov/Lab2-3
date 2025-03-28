(load "MPZ.lisp")

;; Створення фрейму CLIENT
(DEFRAME 'CLIENT
         '(NAME ($VALUE "Іванов Іван"))
         '(AGE ($VALUE 30))
         '(CITY ($VALUE "Київ")))

;; Тест FASSERT
(format t "~%=== Testing FASSERT ===")
(FASSERT 'CLIENT '(NAME ($VALUE "Петров Петро")))
(format t "~%FGETV NAME: ~a (Expected: (\"Петров Петро\"))" (FGETV 'CLIENT 'NAME))

;; Тест FASSERTQ
(format t "~%~%=== Testing FASSERTQ ===")
(FASSERTQ 'CLIENT '(AGE ($VALUE 35)))
(format t "~%FGETV AGE: ~a (Expected: (35))" (FGETV 'CLIENT 'AGE))

;; Тест FGETV
(format t "~%~%=== Testing FGETV ===")
(format t "~%FGETV CITY: ~a (Expected: (\"Київ\"))" (FGETV 'CLIENT 'CITY))

;; Тест FGET
(format t "~%~%=== Testing FGET ===")
(format t "~%FGET NAME $VALUE: ~a (Expected: (\"Петров Петро\"))" 
        (FGET 'CLIENT 'NAME '$VALUE))

;; Тест FPUTV
(format t "~%~%=== Testing FPUTV ===")
(FPUTV 'CLIENT 'CITY "Львів")
(format t "~%After FPUTV, FGETV CITY: ~a (Expected: (\"Львів\"))" 
        (FGETV 'CLIENT 'CITY))

;; Тест FPUT
(format t "~%~%=== Testing FPUT ===")
(FPUT 'CLIENT 'AGE '$VALUE 40)
(format t "~%After FPUT, FGETV AGE: ~a (Expected: (40))" (FGETV 'CLIENT 'AGE))

;; Тест FDELETE
(format t "~%~%=== Testing FDELETE ===")
(FDELETE 'CLIENT 'NAME '$VALUE "Петров Петро")
(format t "~%After FDELETE, FGETV NAME: ~a (Expected: NIL)" (FGETV 'CLIENT 'NAME))

;; Тест FMATCH
(format t "~%~%=== Testing FMATCH ===")
(DEFRAME 'CLIENT2
         '(NAME ($VALUE "Сидоров Сидір"))
         '(AGE ($VALUE 40))
         '(CITY ($VALUE "Львів")))

(DEFRAME 'CLIENT3
         '(NAME ($VALUE "Коваленко Костянтин"))
         '(AGE ($VALUE 35))
         '(CITY ($VALUE "Одеса")))

(FMATCH 'CLIENT '(CITY ($VALUE "Львів")))
(format t "~%Matched clients: ~a (Expected: (CLIENT2))" 
        (mapcar #'frame-data-name (FMATCHED)))
(format t "~%Unmatched clients: ~a (Expected: (CLIENT CLIENT3))" 
        (mapcar #'frame-data-name (FUNMATCHED)))

;; Тест FPRINT для одного фрейму
(format t "~%~%=== Testing FPRINT for single frame ===")
(format t "~%CLIENT frame contents:")
(FPRINT (list (assoc 'CLIENT *FRAMES*)))

;; Тест FPRINT для всіх фреймів
(format t "~%~%=== Testing FPRINT for all frames ===")
(format t "~%All frames in system:")
(FPRINT)
