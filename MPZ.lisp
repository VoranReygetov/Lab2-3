;;1. Constants and Global Variables
(defvar *AKO-INSTANCE* '((AKO INSTANCE) (INSTANCE AKO)) "List of properties for storing inverse relation names")
(defconstant AKO 'AKO)
(defconstant INSTANCE 'INSTANCE)
(defconstant $VALUE '$VALUE)
(defconstant $DEFAULT '$DEFAULT)
(defconstant $IF-NEEDED '$IF-NEEDED)
(defconstant $IF-REMOVED '$IF-REMOVED)
(defconstant $IF-ADDED '$IF-ADDED)
(defconstant $IF-INSTANTIATED '$IF-INSTANTIATED)
(defconstant $REQUIRE '$REQUIRE)
(defconstant $IF-GETED '$IF-GETED)
(defconstant $IF-ADD '$IF-ADD)
(defconstant $IF-REM '$IF-REM)
(defconstant $IF-GET '$IF-GET)

(defvar *FRAMES* nil "List of all frames in the system")
(defvar *PROCEDURES* nil "List of all procedures in the system")
(defvar *FGENAMELIST* nil "Information for generating unique frame names")
(defvar *FRAME-ENV* nil "Environment for frame operations")
(defvar *FRAME* nil "Frame body immediately after FRAME? or FNAME?")
(defvar *FNAME* nil "Frame name immediately after FRAME? or FNAME?")
(defvar *DESCR* nil "Descriptor body immediately after DNAME?")
(defvar *DNAME* nil "Descriptor name immediately after DNAME?")
(defvar *MATCHED* nil "List of objects that were matched after FMATCH?")
(defvar *UNMATCHED* nil "List of objects that were not matched after FMATCH?")

(defstruct frame-data
  name
  slots)

(defstruct slot-data
  name
  facets)

(defstruct facet-data
  name
  values)

;;2. Frame Network and Procedure Modification:

(defun DEFRAME (frame-name &rest slots)
  "Create a new frame with the given name and slots"
  (when (assoc frame-name *FRAMES*)
    (error "Frame ~S already exists" frame-name))
  (let ((frame (make-frame-data 
                :name frame-name
                :slots (mapcar
                        (lambda (slot)
                          (make-slot-data 
                           :name (car slot)
                           :facets (mapcar
                                    (lambda (facet)
                                      (make-facet-data
                                       :name (car facet)
                                       :values (cdr facet)))
                                    (cdr slot))))
                        slots))))
    (push (cons frame-name frame) *FRAMES*)
    frame-name))

(defun DEFRAMEQ (frame-name &rest slots)
  "Create a new frame with quoted slot values"
  (when (assoc frame-name *FRAMES*)
    (error "Frame ~S already exists" frame-name))
  (let ((frame (make-frame-data 
                :name frame-name
                :slots (mapcar
                        (lambda (slot)
                          (make-slot-data 
                           :name (car slot)
                           :facets (mapcar
                                    (lambda (facet)
                                      (make-facet-data
                                       :name (car facet)
                                       :values (list (cadr facet))))
                                    (cdr slot))))
                        slots))))
    (push (cons frame-name frame) *FRAMES*)
    frame-name))

(defun FASSERT (frame-name &rest slots)
  "Add slots to an existing frame or create a new one"
  (let ((frame (cdr (assoc frame-name *FRAMES*))))
    (if frame
        (dolist (slot slots)
          (FPUT frame-name (car slot) (caadr slot) (cadadr slot)))
        (progn
          (DEFRAME frame-name)
          (dolist (slot slots)
            (FPUT frame-name (car slot) (caadr slot) (cadadr slot)))))
    frame-name))

(defun FASSERTQ (frame-name &rest slots)
  "Add quoted slots to an existing frame or create a new one"
  (let ((frame (cdr (assoc frame-name *FRAMES*))))
    (if frame
        (dolist (slot slots)
          (FPUT frame-name (car slot) (caadr slot) (cadadr slot)))
        (progn
          (DEFRAMEQ frame-name)
          (dolist (slot slots)
            (FPUT frame-name (car slot) (caadr slot) (cadadr slot)))))
    frame-name))

(defun FRENAME (old-name new-name)
  "Rename a frame"
  (let ((frame (assoc old-name *FRAMES*)))
    (unless frame
      (error "Frame ~S not found" old-name))
    (setf (car frame) new-name)
    (setf (frame-data-name (cdr frame)) new-name)
    new-name))

(defun FNAME (frame-or-name)
  "Get frame name"
  (if (symbolp frame-or-name)
      (if (assoc frame-or-name *FRAMES*)
          frame-or-name
          (DEFRAME frame-or-name))
      (frame-data-name frame-or-name)))

(defun FRAME (frame-or-name)
  "Get frame data structure"
  (if (symbolp frame-or-name)
      (or (cdr (assoc frame-or-name *FRAMES*))
          (cdr (assoc (FNAME frame-or-name) *FRAMES*)))
      frame-or-name))

(defun FPUT- (frame-name slot-name facet-name value &optional label message)
  "Internal frame put operation without triggering procedures"
  (let ((frame (cdr (assoc frame-name *FRAMES*))))
    (unless frame
      (error "Frame ~S not found" frame-name))
    (let ((slot (find slot-name (frame-data-slots frame) :key #'slot-data-name :test #'eq)))
      (unless slot
        (setf slot (make-slot-data :name slot-name :facets nil))
        (push slot (frame-data-slots frame)))
      (let ((facet (find facet-name (slot-data-facets slot) :key #'facet-data-name :test #'eq)))
        (unless facet
          (setf facet (make-facet-data :name facet-name :values nil))
          (push facet (slot-data-facets slot)))
        (if (and label message)
            (push (list value (list label message)) (facet-data-values facet))
            (push value (facet-data-values facet)))))
    value)
)

(defun FPUT (frame-name slot-name facet-name value &optional label message)
  "Put a value into a frame's slot facet, triggering procedures"
  (let ((result (FPUT- frame-name slot-name facet-name value label message)))
    (let ((if-added (FGET frame-name slot-name '$IF-ADDED))
          (*FRAME-ENV* (list (cons ':FRAME frame-name)
                             (cons ':SLOT slot-name)
                             (cons ':FACET facet-name)
                             (cons ':VALUE value))))
      (dolist (proc if-added)
        (eval proc)))
    result))

(defun FPUTV (frame-name slot-name value)
  "Put a value into a frame's slot value facet"
  (FPUT frame-name slot-name '$VALUE value))

(defun FDELETE (frame-name slot-name facet-name value &optional label message)
  "Delete a value from a frame's slot facet"
  (let ((frame (cdr (assoc frame-name *FRAMES*))))
    (unless frame
      (error "Frame ~S not found" frame-name))
    (let ((slot (find slot-name (frame-data-slots frame) :key #'slot-data-name :test #'eq)))
      (unless slot
        (return-from FDELETE nil))
      (let ((facet (find facet-name (slot-data-facets slot) :key #'facet-data-name :test #'eq)))
        (unless facet
          (return-from FDELETE nil))
        (setf (facet-data-values facet)
              (remove value (facet-data-values facet) :test #'equal))
        (let ((if-removed (FGET frame-name slot-name '$IF-REMOVED)))
          (setf *FRAME-ENV* (list (cons ':FRAME frame-name)
                                   (cons ':SLOT slot-name)
                                   (cons ':FACET facet-name)
                                   (cons ':VALUE value)))
          (dolist (proc if-removed)
            (eval proc)))
        value)))
)

(defun FGET (frame-name slot-name &optional facet-name)
  "Get values from a frame's slot facet"
  (let ((frame (cdr (assoc frame-name *FRAMES*))))
    (unless frame
      (error "Frame ~S not found" frame-name))
    (let ((slot (find slot-name (frame-data-slots frame) :key #'slot-data-name :test #'eq)))
      (unless slot
        (return-from FGET nil))
      (let ((facet (find (or facet-name '$VALUE) (slot-data-facets slot) :key #'facet-data-name :test #'eq)))
        (unless facet
          (return-from FGET nil))
        (let ((if-geted (FGET frame-name slot-name '$IF-GETED))
              (*FRAME-ENV* (list (cons ':FRAME frame-name)
                                 (cons ':SLOT slot-name)
                                 (cons ':FACET (or facet-name '$VALUE))
                                 (cons ':VALUE (facet-data-values facet)))))
          (dolist (proc if-geted)
            (eval proc)))
        (facet-data-values facet)))))

(defun FGETV (frame-name slot-name)
  "Get values from a frame's slot value facet"
  (FGET frame-name slot-name '$VALUE))

(defun FINSTANCE (new-frame-name old-frame-name &rest slots)
  "Create a new frame instance based on an existing frame"
  (let ((old-frame (cdr (assoc old-frame-name *FRAMES*))))
    (unless old-frame
      (error "Frame ~S not found" old-frame-name))
    (let ((new-frame (apply #'DEFRAME (cons new-frame-name slots))))
      (dolist (slot (frame-data-slots old-frame))
        (let ((slot-name (slot-data-name slot))
              (facets (slot-data-facets slot)))
          (dolist (facet facets)
            (let ((facet-name (facet-data-name facet)))
              (dolist (value (facet-data-values facet))
                (FPUT new-frame-name slot-name facet-name value))))))
      (let ((if-instantiated (FGET old-frame-name '$IF-INSTANTIATED)))
        (setf *FRAME-ENV* (list (cons ':NEW-FRAME new-frame-name)
                                (cons ':OLD-FRAME old-frame-name)))
        (dolist (proc if-instantiated)
          (eval proc)))
      new-frame-name)))

(defun DEDESCR (descriptor-name &rest slots)
  "Create a new descriptor frame"
  (apply #'DEFRAME (cons descriptor-name slots)))

(defun FMATCH (frame-name &rest slots)
  "Match frames based on slot values"
  (let ((matched nil)
        (unmatched nil))
    (dolist (frame-pair *FRAMES*)
      (let ((frame (cdr frame-pair)))
        (when (frame-data-p frame) ; Перевірка, чи це структура FRAME-DATA
          (let ((match t))
            (dolist (slot slots)
              (let ((slot-name (car slot))
                    (facet-name (cadr slot))
                    (value (caddr slot)))
                (unless (equal (FGET (frame-data-name frame) slot-name facet-name) value)
                  (setf match nil))))
            (if match
                (push frame matched)
                (push frame unmatched))))))
    (setf *MATCHED* matched)
    (setf *UNMATCHED* unmatched)
    matched))

(defun FMATCHED ()
  "Get matched frames from last FMATCH operation"
  *MATCHED*)

(defun FUNMATCHED ()
  "Get unmatched frames from last FMATCH operation"
  *UNMATCHED*)

;;3. Utility Functions:

(defun FGENNAME ()
  "Generate a unique frame name"
  (let ((new-name (gensym "FRAME-")))
    (push new-name *FGENAMELIST*)
    new-name))

(defun FGENNAMELIST ()
  "Get list of generated frame names"
  *FGENAMELIST*)

; (defun FDISPLAY (frame-name)
;   "Display a frame's contents"
;   (let ((frame (cdr (assoc frame-name *FRAMES*))))
;     (unless frame
;       (error "Frame ~S not found" frame-name))
;     (format t "Frame: ~S~%" frame-name)
;     (dolist (slot (frame-data-slots frame))
;       (format t "  Slot: ~S~%" (slot-data-name slot))
;       (dolist (facet (slot-data-facets slot))
;         (format t "    Facet: ~S, Values: ~S~%" (facet-data-name facet) (facet-data-values facet)))))
; )
; (defun FDISPLAY-ALL ()
;   "Display all frames in the system"
;   (dolist (frame *FRAMES*)
;     (FDISPLAY (car frame))))

(defun FPRINT (&optional (fnl *FRAMES*))
  "Display specified frames in a user-friendly format. Default is *FRAMES*"
  (let ((printed-frames nil))
    (dolist (frame-pair fnl)
      (let* ((frame-name (if (consp frame-pair) (car frame-pair) frame-pair))
             (frame (cdr (assoc frame-name *FRAMES*))))
        (when frame
          (format t "Frame: ~S~%" frame-name)
          (dolist (slot (frame-data-slots frame))
            (format t "  Slot: ~S~%" (slot-data-name slot))
            (dolist (facet (slot-data-facets slot))
              (format t "    Facet: ~S, Values: ~S~%"
                      (facet-data-name facet)
                      (facet-data-values facet))))
          (push frame-name printed-frames))))
    (reverse printed-frames)))

(defun MY-PPRINT (&optional pnl *PROCEDURES*)
  "Display specified procedures in a user-friendly format. Default is *PROCEDURES*"
  (let ((printed-procedures nil))
    (dolist (proc pnl)
      (format t "Procedure: ~S~%" proc)
      (push proc printed-procedures))
    (reverse printed-procedures)))