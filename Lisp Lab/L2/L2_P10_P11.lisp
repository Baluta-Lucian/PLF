; 10. Se da un arbore de tipul (2). Sa se precizeze nivelul pe care apare un nod
; x in arbore. Nivelul radacii se considera a fi 0.  

(defun nivelX (l x n)
    (cond
        ((null l) 0)
        ((equal (car l) x) n)
        (t (max (nivelX (cadr l) x (+ n 1)) (nivelX (caddr l) x (+ n 1))))
    )
)


(nivelX '(a (b(c(i) ()) (f (g) ())) (d (e) (h))) `h `0)

(a (b(c(i) ()) (f (g) ())) (d (e) (h)))

; 11. Se da un arbore de tipul (2).
;  Sa se afiseze nivelul (si lista corespunza-toarea nodurilor) avand numar maxim de noduri. 
;  Nivelul rad. se considera 0.

(defun maxLevel (l n)
    (cond
        ((null l) n)
        (t (max (maxLevel (cadr l) (+ n 1)) (maxLevel (caddr l) (+ n 1))))
    )
)

(defun NodesOnLvl (l currentN n)
    (cond
        ((null l) 0)
        ((= currentN n) 1)
        (t (+ (NodesOnLvl (cadr l) (+ currentN 1) n) (NodesOnLvl (caddr l) (+ currentN 1) n)))
    )
)

(defun noNodesAux (l LevelMax currentLvl mostNodesLvl NodesMax)
    (cond
        ((= currentLvl levelmax) mostNodesLvl)
        ((> (NodesOnLvl l `0 currentLvl) NodesMax) (noNodesAux l LevelMax (+ 1 currentLvl) currentLvl (NodesOnLvl l `0 currentLvl)))
        (t (noNodesAux l LevelMax (+ currentLvl 1) mostNodesLvl NodesMax))
    )
)

(defun buildList (l currentLvl n)
    (cond
        ((null l) `())
        ((= currentLvl n) (list (car l)))
        (t (append (buildList (cadr l) (+ 1 currentLvl) n) (buildList (caddr l) (+ 1 currentLvl) n)))
    )
)

(defun noLevelMax (l)
    (noNodesAux l (maxLevel l `0) `0 `0 `0)
)

(defun MaxNodesList (l)
    (buildList l `0 (noLevelMax l))
)

(noLevelMax '(a (b(c(i) ()) (f (g) ())) (d (e) (h))) )
(MaxNodesList '(a (b(c(i) ()) (f (g) ())) (d (e) (h))))