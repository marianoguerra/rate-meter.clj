(ns marianoguerra.rate-meter-test
  (:require 
    [clj-time.core :as time])
  (:use clojure.test
        marianoguerra.rate-meter))

(def t (time/date-time 2013 04 05 11 02 50))
(def t1 (time/date-time 2013 04 05 11 22 50))
(def t2 (time/date-time 2013 04 05 11 36 50))
(deftest rate-meter-test
  (testing "minute formatter formats correctly"
    (is (= (minute-mark t) "2013-04-05T11:02")))
  
  (testing "minute step mark formats correclty"
    (is (= ((minute-step-mark 5) t) "2013-04-05T11:00"))
    (is (= ((minute-step-mark 7) t1) "2013-04-05T11:21"))
    (is (= ((minute-step-mark 10) t1) "2013-04-05T11:20"))
    (is (= ((minute-step-mark 15) t1) "2013-04-05T11:15"))
    (is (= ((minute-step-mark 25) t1) "2013-04-05T11:00"))
    (is (= ((minute-step-mark 25) t2) "2013-04-05T11:25"))
    (is (= ((minute-step-mark 30) t2) "2013-04-05T11:30")))
  
  (testing "memory-rate-store"
    (let [markers {:one-min minute-mark
                   :five-mins (minute-step-mark 5)}
          store (memory-rate-store markers)
          two-mins-future (time/plus (time/now) (time/minutes 2))]
      (is (= (rate store "mariano" :one-min) 0))
      (is (= (rate store "mariano" :one-min 42) 42))

      (mark store "mariano")
      (is (= (rate store "mariano" :one-min) 1))
      (is (= (rate store "mariano" :five-mins) 1))
      
      (mark store "javier" 5)
      (is (= (rate store "javier" :one-min) 5))
      (is (= (rate store "javier" :five-mins) 5))

      (mark store "javier" 4)
      (is (= (rate store "javier" :one-min) 9))
      (is (= (rate store "javier" :five-mins) 9))

      (mark store "javier" 2 two-mins-future)
      (is (= (rate store "javier" :one-min 0 two-mins-future) 2))
      (is (= (rate store "javier" :five-mins 0 two-mins-future) 11)))))
