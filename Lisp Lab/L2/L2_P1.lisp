; De exemplu arborele
;      A
;     / \
;    B   C
;       / \
;      D   E 
; se poate reprezenta astfel in cele doua moduri:
; (A 2 B 0 C 2 D 0 E 0)       (1)   (nod nr-subarbori lista-subarbore-1 lista-subarbore-2 ...)
; (A (B) (C (D) (E)))         (2)   (nod (lista-subarbore-1) (lista-subarbore-2))

; 1. Se da un arbore de tipul (1). Sa se afiseze calea de la radacina pana la un nod x dat.

;Model matematic:
    ; caleX(l1..ln, x, c):
    ;     (), n = 0
    ;     (), l1 == 0
    ;     c, l1 == x
    ;     caleX(l2..ln, x, l1+c)   , l1 == 1
    ;     caleX(caleX(l2..ln, x, )

(defun caleX(l, x, c) 
    (cond
        ((null l) f)

))