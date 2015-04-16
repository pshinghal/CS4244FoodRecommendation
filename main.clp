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

(defrule one-option-or-less
    (declare (salience 90))
    ?check <- (check-remaining)
    =>
    (switch (length$ (find-all-facts ((?f dish)) (eq 1 1)))
        (case 1 then
            (bind ?dish (find-fact ((?n dish)) (eq 1 1)))
            (bind ?name (fact-slot-value (nth$ 1 ?dish) name))
            (printout t "You should have " ?name crlf))
        (case 0 then
            (printout t "Oops, I don't have any dish suitable for you! Let's try that again." crlf)
            (reset)))
    (retract ?check))

(defrule ask-cuisine
    (declare (salience 85))
    ?state <- (state question)
    (not (asked cuisine))
    =>
    (printout t "Do you prefer any particular cuisine? (None/Chinese/Malay/Indian/Seafood/Western/Japanese/CrossCultural)" crlf)
    (bind ?response (read))
    (switch ?response
        (case Chinese then (assert (preference (property cuisine) (symbolValue Chinese))))
        (case Malay then (assert (preference (property cuisine) (symbolValue Malay))))
        (case Indian then (assert (preference (property cuisine) (symbolValue Indian))))
        (case Seafood then (assert (preference (property cuisine) (symbolValue Seafood))))
        (case Western then (assert (preference (property cuisine) (symbolValue Western))))
        (case Japanese then (assert (preference (property cuisine) (symbolValue Japanese))))
        (case CrossCultural then (assert (preference (property cuisine) (symbolValue CrossCultural))))
        (default (printout t "Great, so we'll search across all cuisines!" crlf)))
    (assert (asked cuisine)))

(defrule process-cuisine
    (declare (salience 80))
    (preference (property cuisine) (symbolValue ?preferred))
    ?dish <- (dish (cuisine ?found))
    =>
    (if (not (eq ?preferred ?found))
    then
        (retract ?dish)))

(defrule check-after-cuisine
    (declare (salience 78))
    (preference (property cuisine))
    (not (checked-after cuisine))
    =>
    (assert (check-remaining))
    (assert (checked-after cuisine)))

(defrule ask-vegetarian
    (declare (salience 75))
    ?state <- (state question)
    (not (asked vegetarian))
    =>
    (printout t "Do you prefer vegetarian food? (Yes/No/Either)" crlf)
    (bind ?response (read))
    (switch ?response
        (case Yes then (assert (preference (property vegetarian) (symbolValue TRUE))))
        (case No then (assert (preference (property vegetarian) (symbolValue FALSE))))
        (default (printout t "Great, so we'll search across vegetarian AND non-vegetarian dishes!" crlf)))
    (assert (asked vegetarian)))

(defrule process-vegetarian
    (declare (salience 70))
    (preference (property vegetarian) (symbolValue ?preferred))
    ?dish <- (dish (vegetarian ?found))
    =>
    (if (not (eq ?preferred ?found))
    then
        (retract ?dish)))

(defrule check-after-vegetarian
    (declare (salience 68))
    (preference (property vegetarian))
    (not (checked-after vegetarian))
    =>
    (assert (check-remaining))
    (assert (checked-after vegetarian)))

(defrule ask-taste
    (declare (salience 65))
    (state question)
    (not (asked taste))
    =>
    (printout t "Do you have particular taste preferences? (Yes/No)" crlf)
    (bind ?response (read))
    (if (eq ?response Yes)
    then
        (printout t "Got it. Let me ask you a few more questions about your taste preferences." crlf)
        (assert (explore taste)))
    (assert (asked taste)))

(defrule ask-spiciness
    (declare (salience 60))
    (state question)
    (not (asked spiciness))
    (explore taste)
    =>
    (printout t "On a scale of 0-4, what's your tolerance (or preference) for spiciness?" crlf)
    (bind ?response (read))
    (switch ?response
        (case 0 then
            (printout t "Ah, so you can't take spicy food. Got it." crlf)
            (assert (preference (property spiciness) (numberValue 0))))
        (case 1 then
            (printout t "Okay, so you have a pretty low tolerance for spiciness." crlf)
            (assert (preference (property spiciness) (numberValue 1))))
        (case 2 then
            (printout t "I see. You can take moderately spicy food." crlf)
            (assert (preference (property spiciness) (numberValue 2))))
        (case 3 then
            (printout t "Ah, so you like pretty spicy food. Cool." crlf)
            (assert (preference (property spiciness) (numberValue 3))))
        (case 4 then
            (printout t "I bow to thee, oh mighty dragon." crlf)
            (assert (preference (property spiciness) (numberValue 4))))
        (default
            (printout t "Didn't catch that. I'm going to assume you're a 2." crlf)
            (assert (preference (property spiciness) (numberValue 2)))))
    (assert (asked spiciness)))

(defrule process-spiciness
    (declare (salience 58))
    (preference (property spiciness) (numberValue ?preferred))
    ?dish <- (dish (spiciness ?found))
    =>
    (if (> ?found ?preferred)
    then
        (retract ?dish)))

(defrule check-after-spiciness
    (declare (salience 56))
    (preference (property spiciness))
    (not (checked-after spiciness))
    =>
    (assert (check-remaining))
    (assert (checked-after spiciness)))

(defrule ask-sweet
    (declare (salience 54))
    (state question)
    (not (asked sweet))
    (explore taste)
    =>
    (printout t "Do you want to eat something sweet? (Yes/No/Either)" crlf)
    (bind ?response (read))
    (switch ?response
        (case Yes then (assert (preference (property sweet) (symbolValue TRUE))))
        (case No then (assert (preference (property sweet) (symbolValue FALSE)))))
    (assert (asked sweet)))

(defrule process-sweet
    (declare (salience 52))
    (preference (property sweet) (symbolValue ?preferred))
    ?dish <- (dish (sweet ?found))
    =>
    (if (not (eq ?found ?preferred))
    then
        (retract ?dish)))

(defrule check-after-sweet
    (declare (salience 50))
    (preference (property sweet))
    (not (checked-after sweet))
    =>
    (assert (check-remaining))
    (assert (checked-after sweet)))

(defrule ask-sour
    (declare (salience 48))
    (state question)
    (not (asked sour))
    (explore taste)
    =>
    (printout t "Would you like something sour? (Yes/No/Either)" crlf)
    (bind ?response (read))
    (switch ?response
        (case Yes then (assert (preference (property sour) (symbolValue TRUE))))
        (case No then (assert (preference (property sour) (symbolValue FALSE)))))
    (assert (asked sour)))

(defrule process-sour
    (declare (salience 46))
    (preference (property sour) (symbolValue ?preferred))
    ?dish <- (dish (sour ?found))
    =>
    (if (not (eq ?found ?preferred))
    then
        (retract ?dish)))

(defrule check-after-sour
    (declare (salience 44))
    (preference (property sour))
    (not (checked-after sour))
    =>
    (assert (check-remaining))
    (assert (checked-after sour)))

(defrule ask-nutrition
    (declare (salience 35))
    (state question)
    (not (asked nutrition))
    =>
    (printout t "Do you want to specify any nutrition preferences? (Yes/No)" crlf)
    (bind ?response (read))
    (if (eq ?response Yes)
    then
        (printout t "Got it. Let me ask you a few more questions about your nutrition preferences." crlf)
        (assert (explore nutrition)))
    (assert (asked nutrition)))

(defrule ask-lowcal
    (declare (salience 30))
    (state question)
    (not (asked lowcal))
    (explore nutrition)
    =>
    (printout t "Do you prefer food that has low calories? (Yes/No/Either)" crlf)
    (bind ?response (read))
    (switch ?response
        (case Yes then (assert (preference (property lowcal) (symbolValue TRUE))))
        (case No then (assert (preference (property lowcal) (symbolValue FALSE)))))
    (assert (asked lowcal)))

(defrule process-lowcal
    (declare (salience 28))
    (preference (property lowcal) (symbolValue ?preferred))
    ?dish <- (dish (lowcal ?found))
    =>
    (if (not (eq ?found ?preferred))
    then
        (retract ?dish)))

(defrule check-after-lowcal
    (declare (salience 26))
    (preference (property lowcal))
    (not (checked-after lowcal))
    =>
    (assert (check-remaining))
    (assert (checked-after lowcal)))

(defrule ask-lowna
    (declare (salience 24))
    (state question)
    (not (asked lowna))
    (explore nutrition)
    =>
    (printout t "Do you prefer food that has low sodium? (Yes/No/Either)" crlf)
    (bind ?response (read))
    (switch ?response
        (case Yes then (assert (preference (property lowna) (symbolValue TRUE))))
        (case No then (assert (preference (property lowna) (symbolValue FALSE)))))
    (assert (asked lowna)))

(defrule process-lowna
    (declare (salience 22))
    (preference (property lowna) (symbolValue ?preferred))
    ?dish <- (dish (lowna ?found))
    =>
    (if (not (eq ?found ?preferred))
    then
        (retract ?dish)))

(defrule check-after-lowna
    (declare (salience 20))
    (preference (property lowna))
    (not (checked-after lowna))
    =>
    (assert (check-remaining))
    (assert (checked-after lowna)))

(defrule ask-lowfat
    (declare (salience 18))
    (state question)
    (not (asked lowfat))
    (explore nutrition)
    =>
    (printout t "Do you prefer food that has low fat? (Yes/No/Either)" crlf)
    (bind ?response (read))
    (switch ?response
        (case Yes then (assert (preference (property lowfat) (symbolValue TRUE))))
        (case No then (assert (preference (property lowfat) (symbolValue FALSE)))))
    (assert (asked lowfat)))

(defrule process-lowfat
    (declare (salience 16))
    (preference (property lowfat) (symbolValue ?preferred))
    ?dish <- (dish (lowfat ?found))
    =>
    (if (not (eq ?found ?preferred))
    then
        (retract ?dish)))

(defrule check-after-lowfat
    (declare (salience 14))
    (preference (property lowfat))
    (not (checked-after lowfat))
    =>
    (assert (check-remaining))
    (assert (checked-after lowfat)))

(defrule ask-highfiber
    (declare (salience 12))
    (state question)
    (not (asked highfiber))
    (explore nutrition)
    =>
    (printout t "Do you prefer food that has high fiber? (Yes/No/Either)" crlf)
    (bind ?response (read))
    (switch ?response
        (case Yes then (assert (preference (property highfiber) (symbolValue TRUE))))
        (case No then (assert (preference (property highfiber) (symbolValue FALSE)))))
    (assert (asked highfiber)))

(defrule process-highfiber
    (declare (salience 10))
    (preference (property highfiber) (symbolValue ?preferred))
    ?dish <- (dish (highfiber ?found))
    =>
    (if (not (eq ?found ?preferred))
    then
        (retract ?dish)))

(defrule check-after-highfiber
    (declare (salience 8))
    (preference (property highfiber))
    (not (checked-after highfiber))
    =>
    (assert (check-remaining))
    (assert (checked-after highfiber)))
