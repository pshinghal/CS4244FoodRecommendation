(defrule start
    ?state <- (state start)
    =>
    (printout t crlf)
    (printout t "*********************************************************************************" crlf)
    (printout t "Welcome to the world's most advanced food recommendation system [citation needed]" crlf)
    (printout t "*********************************************************************************" crlf)
    (printout t crlf)
    (retract ?state)
    (assert (state question))))

(deffacts start
    (state start))

(defrule terminate
    (declare (salience 99))
    (state terminated)
    =>
    (printout t "Would you like to try that again? (Yes/No)" crlf)
    (bind ?response (read))
    (if (eq ?response Yes)
    then
        (reset)
    else
        (halt)))

(defrule suggest
    (declare (salience 95))
    ?state <- (state suggest)
    =>
    (bind ?dishes (find-all-facts ((?f dish)) (eq 1 1)))
    (if (eq (length$ ?dishes) 0)
    then
        (printout t "Sorry, I can't find any dishes you'd like!" crlf)
        (retract ?state)
        (assert (state terminated))
    else
        (bind ?somedish (nth$ (+ 1 (mod (random) (length$ ?dishes))) ?dishes))
        (bind ?name (fact-slot-value ?somedish name))
        (printout t "I'd suggest you have " ?name ". Does that sound good? (Yes/No)" crlf)
        (bind ?response (read))
        (if (eq ?response Yes)
        then
            (printout t "Great! Remember me the next time you're hungry." crlf)
            (retract ?state)
            (assert (state terminated))
        else
            (retract ?somedish)
            (retract ?state)
            (assert (state question)))))

(defrule one-option-or-less
    (declare (salience 90))
    ?check <- (check-remaining)
    ?state <- (state question)
    =>
    (if (< (length$ (find-all-facts ((?f dish)) (eq 1 1))) 2)
    then
        (retract ?state)
        (assert (state suggest)))
    (retract ?check))
