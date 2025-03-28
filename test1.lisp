(load "FBZ.lisp")
;; Додавання клієнта
(FASSERT 'Client '(Registr ($VALUE 1234567890))
                 '(Address ($VALUE "вул. Іванова, 1"))
                 '(Name ($VALUE "Іванов Іван Петрович"))
                 '(Phone ($VALUE "0951234567")))

;; Додавання квартири
(FASSERT 'Apartment '(ApartmentID ($VALUE 1))
                   '(AddressFlat ($VALUE "вул. Шевченка, 10"))
                   '(District ($VALUE "Шевченківський"))
                   '(Floors ($VALUE 9))
                   '(AtFloor ($VALUE 5))
                   '(TypeHouse ($VALUE "цегляний"))
                   '(TypePlan ($VALUE "нова"))
                   '(TypeToilet ($VALUE "роздільний"))
                   '(SqAll ($VALUE 70))
                   '(SqLife ($VALUE 40))
                   '(SqKit ($VALUE 10))
                   '(AgentID ($VALUE "agent1"))
                   '(Privat ($VALUE t))
                   '(SignPhone ($VALUE t))
                   '(Document ($VALUE "Свідоцтво про право власності")))

;; Додавання договору
(FASSERT 'Treaty '(TreatyID ($VALUE 100))
                   '(ClientID ($VALUE 1234567890))
                   '(ApartmentID ($VALUE 1))
                   '(DateStart ($VALUE "2023-10-26"))
                   '(StopDate ($VALUE "2024-10-26"))
                   '(Cost ($VALUE 100000))
                   '(Bonus ($VALUE 5000)))

;; Виведення даних клієнта
(FPRINT 'Client)

;; Отримання імені клієнта
(print (FGETV 'Client 'Name))

;; Пошук квартир в Шевченківському районі
(FMATCH 'Apartment '(District ($VALUE "Шевченківський")))
(FPRINT)