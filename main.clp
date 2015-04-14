(defrule start
    ?state <- (state start)
    =>
    (printout t crlf)
    (printout t "Welcome to the world's most advanced food recommendation system [citation needed]" crlf)
    (printout t crlf)
    (retract ?state)
    (assert (state question))))

(deffacts start
    (state start))
