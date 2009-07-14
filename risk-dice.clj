(ns
  my-prob-test
  (:use [clojure.contrib.probabilities.finite-distributions
   :only (uniform prob cond-prob join-with dist-m choose
          normalize certainly cond-dist-m normalize-cond)])
  (:use [clojure.contrib.monads
   :only (domonad with-monad m-seq m-chain m-lift)])
  (:require clojure.contrib.accumulators))

(defn make-die [low high]
  "Create fair die"
  (uniform (set (range low (+ 1 high)))))

(defn make-dice [low high num]
  (loop [a-dice [] i num]
		(if (zero? i)
		  a-dice
		  (recur (conj a-dice (make-die low high)) (dec i)))))

;; look up how to do comparisons in the source file

(defn roll-prob [an dn al ah dl dh]
  "Calculates prob of rolls given ties go to the defender"
  (let [a-die (make-die al ah)
	d-die (make-die dl dh)]
    (cond
      (= an dn 1)
      (one-v-one a-die d-die)
      (= an 1)
      (one-v-two a-die d-die)
      (= an dn 2)
      (two-v-two a-die d-die)
      (= an 2)
      (two-v-one a-die d-die)
      (and (= dn 2) (= an 3))
      (three-v-two a-die d-die)
      (and (= dn 1) (= an 3))
      (three-v-one a-die d-die))))

(defn any-v-any [an dn a-die d-die]
  "Calculates rolls for any number of attackers or defenders"
  (domonad dist-m
	   []
	   ))

(defn one-v-one [a-die d-die]
  (domonad dist-m
	   [d1 a-die
	    d2 d-die]
	   (if (> d1 d2) "attacker wins"
	       "defender wins")))

(defn one-v-two [a-die d-die]
  (domonad dist-m
	   [d1 a-die
	    d2 d-die
	    d3 d-die]
	   (if (> d1 d2 d3) "attacker wins"
	       "defender wins")))

(defn two-v-one [a-die d-die]
  (domonad dist-m
	   [d1 a-die
	    d2 a-die
	    d3 d-die]
	   (if (or (> d1 d3) (> d2 d3)) "attacker wins"
	       "defender wins")))

(defn two-v-two [a-die d-die]
  (domonad dist-m
	   [d1 a-die
	    d2 a-die
	    d3 d-die
	    d4 d-die]
	   (cond (or (and (> d1 d3) (> d2 d4))
		     (and (> d1 d4) (> d2 d3)))
		 "attacker wins"
		 (or (and (> d1 d3) (< d2 d4))
		     (and (> d1 d4) (< d2 d3))
		     (and (< d1 d3) (> d2 d4))
		     (and (< d1 d4) (> d2 d3)))
		 "draw"
		 1
		 "defender wins")))

(defn three-v-one [a-die d-die]
  (domonad dist-m
	   [d1 a-die
	    d2 a-die
	    d3 a-die
	    d4 d-die]
	   (if (or (> d1 d4)
		   (> d2 d4)
		   (> d3 d4))
	     "attacker wins"
	     "defender wins")))

(defn three-v-two [a-die d-die]
  (domonad dist-m
	   [d1 a-die
	    d2 a-die
	    d3 a-die
	    d4 d-die
	    d5 d-die]
	   (cond (or (and (> d1 d4) (> d2 d5))
		     (and (> d1 d5) (> d2 d4))
		     (and (> d1 d4) (> d3 d5))
		     (and (> d1 d5) (> d3 d4))
		     (and (> d2 d4) (> d3 d5))
		     (and (> d2 d5) (> d3 d4)))
		 "attacker wins"
		 (or (and (> d1 d4) (< d2 d5) (< d3 d5))
		     (and (> d1 d5) (< d2 d4) (< d3 d4))
		     (and (> d2 d4) (< d1 d5) (< d1 d5))
		     (and (> d2 d5) (< d1 d4) (< d3 d4))
		     (and (> d3 d4) (< d2 d5) (< d1 d5))
		     (and (> d3 d5) (< d2 d4) (< d1 d4)))
		 "draw"
		 1
		 "defender wins")))
