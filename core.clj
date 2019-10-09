(ns microproject2.core)

(defn deep-substitute
  "Given a map of replacement key/value pairs, m, and a list, l, returns a list with values from l,
   but with any elements equal to a key in m replaced with the corresponding val in m.
   If l contains nested lists, recursively performs replacement in those lists as well."
  [l m]
  (map (fn [i]
         (if (seq? i)
           (deep-substitute i m)
           (get m i i)))
       l)
  )

(defn nor-help
  [x]
  (cond
    ;if condition at beginning of list is not change to nor;
    (= 'not (nth x 0)) (conj (rest x) 'nor)

    ;changes and to nor;
    (= 'and (nth x 0)) (map (fn [i]
                              (cond
                                (not= i 'and) (conj (list i) 'nor)
                                (= i 'and) 'nor)
                              ) x)

    ;adds nor to the begining of the list;
    (= 'or (nth x 0)) (conj(list(conj (rest x) 'nor))'nor)
    :else x
    )
  )

(defn nor-convert
  [x]
  (nor-help(doall (map (fn [i]
                         (if (seq? i)
                           (nor-convert i)
                           i)
                         ) x)
                  )
           ))

(defn simplify-help
  [x]
  (cond

    ;if all elements are false returns true;
    (every? false? (rest x))true
    (some true? x)false
    ;removes all false elements from lists;
    ;(nor x false false)->(nor x);
    (some false? x) (simplify-help (remove false? (distinct x)))

    ;when encountering (nor (nor x));
    ;checks if expression given is of length 2;
    ;AND if second item in list is a list;
    ;if so then it returns the item at;
    ;the end of the list;
    ;(seq? (nth x 1)) (nth (nth x 1) 1);
    (and (= 2 (count x)) (seq? (nth x 1)))
    (if (= 2 (count (nth x 1)))
      (nth (nth x 1) 1)
      (distinct x))

    :else (distinct x)
    )
  )

(defn simplify
  [x]
  (simplify-help (doall (map (fn [i]
                               (if (seq? i)
                                 (simplify i)
                                 i))x)))
  )

(defn evalexp
  [exp bindings]
  (simplify (nor-convert (deep-substitute exp bindings)))
  )

(def test1 '(not x))
;-> (nor x);
(def test2 '(and x y))
;-> (nor (nor x) (nor y));
(def test3 '(and x y z))
;-> (nor (nor x) (nor y) (nor z));
(def test4 '(and w x y z))
;-> (nor (nor w) (nor x) (nor y) (nor z));
(def test5 '(or x y))
;-> (nor (nor x y));
(def test6 '(or x y z))
;-> (nor (nor x y z));
(def test7 '(or w x y z))
;-> (nor (nor w x y z));

(def p1 '(and x (or x (and y (not z)))))

(def p2 '(and (and z false) (or x true false)))

(def p3 '(or true a))

(def p4 '(or x y false (and true (not (not (not z))))))