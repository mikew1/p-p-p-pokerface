(ns p-p-p-pokerface)

(defn rank [[rank _]]
  (let [replacements {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit rank)
        (Integer/valueOf (str rank))
        (replacements rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))
;(defn suit [[_ suit]]
;    (str suit))

(defn pair? [hand]
  (let [bare-freqs (vals (frequencies (map rank hand)))]
  (= 2 (apply max bare-freqs))))

(defn three-of-a-kind? [hand]
  (let [bare-freqs (vals (frequencies (map rank hand)))]
  (= 3 (apply max bare-freqs))))

(defn four-of-a-kind? [hand]
  (let [bare-freqs (vals (frequencies (map rank hand)))]
  (= 4 (apply max bare-freqs))))

(defn flush? [hand]
  (apply = (map suit hand)))

(defn full-house? [hand]
  (let [sorted-ranks (sort (map rank hand))
        [a b c d e]  sorted-ranks] ; feel the power
    (or (and (= a b)   (= c d e))
        (and (= a b c) (= d e)))))

(defn two-pairs? [hand]
  (let [sorted-ranks (sort (map rank hand))
        [a b c d e]  sorted-ranks]
    (or (and (= a b) (= c d))
        (and (= a b) (= d e))
        (and (= b c) (= d e)))))

(defn straight? [hand]
  (let [sorted-ranks (sort (map rank hand))
        [a b c d e]  sorted-ranks
        straight (range a (+ a 5))]
    (or (= straight sorted-ranks)
        (= (range 1 6) (sort (replace { 14 1 } sorted-ranks))))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))


(defn value [hand] ; can't just check against the test hands, check all poss. hands
  (let [high-card? (fn [hand] true)
        checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        scores (map
                    (fn [[f s]] (if (f hand) s 0)) ; get the fn f from target,
                    checkers)]                     ;  & eval it, great.
      (apply max scores)))                         ; that's an oo pattern in 1 line.
                                                   ; set of fns is first step,
                                                   ; destruct second, apply third.
