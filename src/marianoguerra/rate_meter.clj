(ns marianoguerra.rate-meter
  (:require
    [clj-time.core :as time]
    [clj-time.format :as time-format]))

(def minute-formatter (time-format/formatter "yyyy-MM-dd'T'hh:mm"))
(defn minute-mark [date-time]
  (time-format/unparse minute-formatter date-time))

(defn minute-step-mark [step]
  (fn [^org.joda.time.DateTime date-time]
    (let [date-mins (time/minute date-time)
          rounded-mins (* (quot date-mins step) step)
          new-date-time (.withMinuteOfHour date-time rounded-mins)]
      (minute-mark new-date-time))))

(defn- merge-mark [old-mark new-mark]
  (if old-mark
    (let [{old-mark-val :mark old-rate :rate} old-mark
          {new-mark-val :mark new-rate :rate} new-mark]
      (if (= old-mark-val new-mark-val)
        {:mark new-mark-val :rate (+ old-rate new-rate)}
        new-mark))
    new-mark))

(defn- merge-marks [old-marks new-marks]
  (if old-marks
    (into {} (map (fn [[tag mark]]
                    [tag (merge-mark (get old-marks tag) mark)])
                  new-marks))
    new-marks))

(defprotocol RateStore
  (mark [this id] [this id num] [this id num date-time])
  (rate [this id tag] [this id tag default] [this is tag default date-time]))

(defrecord MemoryRateStore [store markers]
  RateStore
  (mark [this id]
    (mark this id 1))

  (mark [this id num]
    (mark this id num (time/now)))

  (mark [this id num date-time]
    (let [new-marks (into {} (map
                               (fn [[tag marker-fun]]
                                 [tag {:mark (marker-fun date-time) :rate num}])
                               markers))

          old-marks (get @store id)
          updated-marks (merge-marks old-marks new-marks)]
      (swap! store #(assoc % id updated-marks))))

  (rate [this id tag]
    (rate this id tag 0))

  (rate [this id tag default]
    (rate this id tag default (time/now)))

  (rate [this id tag default date-time]
    ; get the current mark and rate for id and tag
    (let [{:keys [mark rate]} (get-in @store [id tag])
          ; get the marker for the given tag
          marker (get markers tag)
          ; generate the current mark with marker
          current-mark (if marker (marker date-time) nil)]
      ; if there is a rate
      (if rate
        ; and the current mark is the same as the one stored (that is, the
        ; rate is from the same time segment)
        (if (= current-mark mark)
          ; return the rate
          rate 
          ; otherwise, since the mark is outdated, return default
          default)
        ; if there was no rate return default
        default))))

(defn memory-rate-store [markers]
  (MemoryRateStore. (atom {}) markers))
