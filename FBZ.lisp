(load "MPZ.lisp")
;; Створення фреймів
(DEFRAME 'Client
         '(Registr ($VALUE nil))
         '(Address ($VALUE nil))
         '(Name ($VALUE nil))
         '(Phone ($VALUE nil)))

(DEFRAME 'Treaty
         '(TreatyID ($VALUE nil))
         '(ClientID ($VALUE nil))
         '(ApartmentID ($VALUE nil))
         '(DateStart ($VALUE nil))
         '(StopDate ($VALUE nil))
         '(Cost ($VALUE nil))
         '(Bonus ($VALUE nil))
         '(Prolong ($VALUE nil))
         '(Comment ($VALUE nil)))

(DEFRAME 'Apartment
         '(ApartmentID ($VALUE nil))
         '(AddressFlat ($VALUE nil))
         '(District ($VALUE nil))
         '(Floors ($VALUE nil))
         '(AtFloor ($VALUE nil))
         '(TypeHouse ($VALUE nil))
         '(TypePlan ($VALUE nil))
         '(TypeToilet ($VALUE nil))
         '(SqAll ($VALUE nil))
         '(SqLife ($VALUE nil))
         '(SqKit ($VALUE nil))
         '(AgentID ($VALUE nil))
         '(Privat ($VALUE nil))
         '(SignPhone ($VALUE nil))
         '(Picture ($VALUE nil))
         '(Plan ($VALUE nil))
         '(Structure ($VALUE nil))
         '(Document ($VALUE nil)))

(DEFRAME 'Agent
         '(AgentID ($VALUE nil))
         '(Name ($VALUE nil))
         '(Phone ($VALUE nil)))

