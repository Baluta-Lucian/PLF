; Pentru urmatoarele probleme se cer functii Lisp programate in mod recursiv:
; 1.a)Sa se insereze intr-o lista liniara un atom a dat dupa al 2-lea, al 4-lea, al 6-lea,....element.
; b)Definiti o functie care obtine dintr-o lista data lista tuturor atomilor care apar, pe orice nivel, dar in ordine inversa.
;                  De exemplu:(((A B) C) (D E)) --> (E D C B A)
                 
; c)Definiti o functie care intoarce cel mai mare divizor comun al numerelor dintr-o lista neliniara.
; d)Sa se scrie o functie care determina numarul de aparitii ale unui atom dat intr-o lista neliniara.

(defun reverseList (l c)
    (cond 
        ((null l) c)
        (t (reverseList (cdr l) (cons (car l) c)))
    )

)

(defun inserareAtom2(l x poz c)

    (cond
        ((null l) (reverseList c `()))
        ((= poz 2) (inserareAtom2 (cdr l) x 1 (cons x (cons (car l) c))))
        (t (inserareAtom2 (cdr l) x (+ poz 1) (cons (car l) c)))
    )

)

(inserareAtom2 `(2 4 6 8) `A `1 `()) ;uses aux function(reverseList) to reverse the collector list for correct answer

(defun inserareAtomV2 (l x poz c)
    (cond
        ((null l) c)
        ((= poz 2) (inserareAtomV2 (cdr l) x 1 (append c (list (car l)) (list x))))
        (t (inserareAtomV2 (cdr l) x (+ poz 1) (append c (list (car l)))))
    )

)


(inserareAtomV2 `(2 4 6 8) `A `1 `()) ;the function does not need any aux function


; b)Definiti o functie care obtine dintr-o lista data lista tuturor atomilor care apar, pe orice nivel, dar in ordine inversa.
;                  De exemplu:(((A B) C) (D E)) --> (E D C B A)

(defun not-member(l x)
    (cond
        ((null l) t)
        ((equal (car l) x) nil)
        (t (not-member (cdr l) x))
    )

)

(defun unique (l c)
    (cond
        ((null l) c)
        ((not-member c (car l)) (unique (cdr l) (append (list (car l)) c)))
        (t (unique (cdr l) c))
    )

)

(defun make-one (l c)
    (cond
        ((null l) c)
        ((listp (car l)) (make-one (cdr l) (append c (make-one (car l) `()))))
        (t (make-one (cdr l) (append c (list (car l)))))
    )
)

(defun invers-freq (l)
    (unique (make-one l `()) `())
)


(invers-freq `(((A B) C) (D E)))


; c)Definiti o functie care intoarce cel mai mare divizor comun al numerelor dintr-o lista neliniara.

(defun while-aux (copie cmmdc)
    (cond
    ((= copie cmmdc) cmmdc)
    ((> copie cmmdc) (while-aux (- copie cmmdc) cmmdc))
    (t (while-aux copie (- cmmdc copie)))
    )
)

(defun cmmdcList-aux (l cmmdc)
    (cond
        ((null l) cmmdc)
        (t (cmmdcList-aux (cdr l) (while-aux (car l) cmmdc)))
    )
)

(defun cmmdcList (l)
    (cmmdcList-aux (cdr (make-one l `())) (car (make-one l `())))
)

