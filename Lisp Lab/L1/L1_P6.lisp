; 6.
; a) Sa se scrie de doua ori elementul de pe pozitia a n-a a unei liste 
; liniare. De exemplu, pentru (10 20 30 40 50) si n=3 se va produce (10 20 
; 30 30 40 50).
; b) Sa se scrie o functie care realizeaza o lista de asociere cu cele doua
; liste pe care le primeste. De ex: (A B C) (X Y Z) --> ((A.X) (B.Y) 
; (C.Z)).
; c) Sa se determine numarul tuturor sublistelor unei liste date, pe orice 
; nivel. Prin sublista se intelege fie lista insasi, fie un element de pe 
; orice nivel, care este lista. Exemplu: (1 2 (3 (4 5) (6 7)) 8 (9 10)) => 
; 5 (lista insasi, (3 ...), (4 5), (6 7), (9 10)).
; d) Sa se construiasca o functie care intoarce numarul atomilor dintr-o lista, 
; de la nivel superficial.


;;Functie ajutatoare pentru a)
; ;Model matematic:
;     reverseList(l1..ln, c1..cn):
;         c, n = 0
;         reverseList(l2..ln, l1 + c), n > 0
(defun reverseList(l c)
    (cond
        ((null l) c)
        (t ( reverseList (cdr l) (cons (car l) c)))

))


;; SOLVED
; a)
; Model matematic:
;     nTwin(l1l2..ln, n, poz, c1c2..cn):
;             c1c2..cn, len(l) = 0
;             nTwin(l1..ln, n, 0, l1+c), n > 0, poz == n
;             nTwin(l2..ln, n , poz+1, l1+c), n > 0, poz != n

(defun nTwin(l n poz c)
    (cond 
        ((null l) (reverseList c `()))
        ((= poz n) (nTwin l n 0 (cons (car l) c)))
        (t ( nTwin (cdr l) n (incf poz) (cons (car l) c)))
        
))

(nTwin `(10 20 30 40 50) `3 `1 `() )



; ;SOLVED
;;;;; b) Sa se scrie o functie care realizeaza o lista de asociere cu cele doua
; liste pe care le primeste. De ex: (A B C) (X Y Z) --> ((A.X) (B.Y) 
; (C.Z)).

; ;Model matematic
;     asociereLista(l1..ln, L1..Ln, r1..rn):
;         r, n1 = 0 || n2 = 0
;          asociereLista(l2..ln, L2..Ln, (l1.Ln) + c), n1,2 > 0


; constList(l1 l2):
;     (l1.l2)

(defun asociereLista(l1 l2 c)
    (cond
        ((null l1) (reverseList c `() ))
        ((null l2) (reverseList c `() ) )
        (t (asociereLista (cdr l1) (cdr l2) (cons (cons(car l1) (car l2)) c)))
))

(asociereLista `(A B C) `(X Y Z) `())


;;SOLVED
;;; c) Sa se determine numarul tuturor sublistelor unei liste date, pe orice 
; nivel. Prin sublista se intelege fie lista insasi, fie un element de pe 
; orice nivel, care este lista. Exemplu: (1 2 (3 (4 5) (6 7)) 8 (9 10)) => 
; 5 (lista insasi, (3 ...), (4 5), (6 7), (9 10)).

;Model matematic:
    ; noList(l1..ln, nr):
    ;     nr, n = 0
    ;     noList(l2..ln, noList(l1, nr+1)), n > 0, l1 is list
    ;     noList(l2..ln, nr), n > 0, l is not list 


(defun noList(l nr)
    (cond 
        ((null l) nr)
        ((listp (car l)) (noList (cdr l) (noList (car l) (incf nr))))
        (t (noList (cdr l) nr))

))

(noList `(1 2 (3 (4 5) (6 7)) 8 (9 10)) `1)


;;; ; d) Sa se construiasca o functie care intoarce numarul atomilor dintr-o lista, 
; de la nivel superficial.

; Model matematic noAtom(l1..ln, nr):
;     nr, n = 0
;     noAtom(l2..ln, noAtom(l1, nr)), n > 0, l1 is list
;     noAtom(l2..ln, nr+1), n > 0, l1 is atom
;     noAtom(l2..ln, nr), n > 0, daca l1 is not atom

(defun noAtom(l nr)
    (cond
        ((null l) nr)
        ((listp (car l)) (noAtom (cdr l) (noAtom (car l) nr)))
        ((symbolp (car l)) (noAtom (cdr l) (incf nr)))
        (t (noAtom (cdr l) nr))   
))

(noAtom `(A B (2 3 C (D)) 4 (A (C (8) R) D) 15) `0);rezultat 13(nr de elemente din lista)

; (defun noAtom(l nr)
;     (cond
;         ((null l) nr)
;         ((listp (car l)) (noAtom (cdr l) (noAtom (car l) nr)))
;         ((atom (car l)) (noAtom (cdr l) (incf nr)))
;         (t (noAtom (cdr l) nr))   
; ))

; (noAtom `(A B (2 3 C (D)) 4 (A (C (8) R) D) 15) `0) ; rezultat 8(nr de simboluri din lista)