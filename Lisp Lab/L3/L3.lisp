; 11.  Sa se scrie o functie care sterge toate aparitiile unui atom de la toate nivelurile unei liste.

(defun sterge-atom-rec (l atom)
    (mapcar (lambda (x)
        (cond 
            ((eq x atom) `())
            ((listp x) (sterge-atom-rec x atom))
            (t x)
        )) l)

)

(sterge-atom-rec `(((A B) A) (D E)) `A)