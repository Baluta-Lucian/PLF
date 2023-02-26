Sa se construiasca lista nodurilor unui 
arbore de tipul (1) parcurs in inordine.
(A 2 B 0 C 2 D 0 E 0)         (1)


;;Model matematic:
    ; inordine(l1l2l3):
    ;     NIL, daca l este vida
    ;     inordine(l2)+l1+inordine(l3) altfel
    
(defun inordine(arb) 
    (cond
        ((null arb) nil)
        (t (append (inordine (stang arb)) (cons (car arb) (inordine (drept arb)))))
        
        
))    
    

    
; (defun inordine(arb) 
;     (cond
;         ((null arb) nil)
;         (t (append (stang arb) (cons (car arb) (drept arb))))
        
        
;         ))    


;;Model matematic
;nv= numaru; de varfuri, nm= numarul de muchii
; parcurg_st(l1l2..lk, nv, nm):
;         NIL, daca l e vida
;         NIL, daca nv = 1 + nm
;         l1+l2+parcurg_st(l3..lk, nv+1, nm+l2), altfel



(defun parcurg_st(arb nv nm)
    (cond
        ((null arb) nil)
        ((= nv (+ 1 nm)) nil)
        (t (cons (car arb) (cons (cadr arb) (parcurg_st (cddr arb) (+ 1 nv) (+ (cadr arb) nm)))))

))

(defun parcurg_dr(arb nv nm)
    (cond 
        ((null arb) nil)
        ((= nv (+ 1 nm)) arb)
        (t (parcurg_dr (cddr arb) (+ 1 nv) (+ (cadr arb) nm)))

))


(defun stang(arb)
    (parcurg_st (cddr arb) 0 0)
)

(defun drept(arb)
    (parcurg_dr (cddr arb) 0 0)
)


(inordine `(a 2 b 2 c 1 i 0 f 1 g 0 d 2 e 0 h 0) )


