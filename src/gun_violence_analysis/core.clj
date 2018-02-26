;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; exploration of the Mother Jones' US mass shootings data set in
;;; relation to http://wapo.st/2onKxvT?tid=ss_tw-amp~, a proposal to
;;; reinstate the 1994-2004 Federal Assault Weapons Ban in the wake of
;;; Parkland.
;;;
;;; Mother Jones data:
;;; https://www.motherjones.com/politics/2012/12/mass-shootings-mother-jones-full-data/
;;;
;;; Of note:
;;;
;;; - While the Parkland shooter used a civilian-model AR-15
;;;   semi-automatic rifle, the Assault Weapons Ban banned semi-automatic
;;;   rifles, pistols, and shotguns
;;;
;;; - The Washington Post article restricts mass shootings to 6 or more
;;;   _deaths_ but the Mother Jones data does not.
;;;
;;; - The Mother Jones data set is from 1982-2018, containing 56 incidents
;;;   with 6 or more deaths.
;;;
;;; - The Mother Jones data set is not overly scientific in its
;;;   description of the weapons involved. For the purposes of this
;;;   exploration, I'll consider any occurence of the regex
;;;   '(semi-?automatic|assault)' to indicate that it would have been
;;;   covered in the ban. That provides 44 incidents.
;;;
;;; - The Washington Post article documents data in 10 year increments
;;;   from 1984 to 2014. Thus we shold drop data outside that range. That
;;;   provides 40 incidents.
;;;
;;; - I'm assuming that the data was anaylzed disjointly for each of the
;;;   decade ranges.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ns gun-violence-analysis.core
  (:gen-class)
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(def raw (with-open [i (io/reader "Mother Jones' Investigation_ US Mass Shootings, 1982-2018 - US mass shootings.csv")]
           (doall (csv/read-csv i))))

(defn raw->map
  [[head & rest :as raw]]
  (map (partial zipmap head) rest))

(defn get-year
  [incident-map]
  (Integer/parseInt (get incident-map "Year")))

(defn get-type-of-weapons
  [incident-map]
  (get incident-map "Type of weapons"))

(defn get-fatalities
  [incident-map]
  (Integer/parseInt (get incident-map "Fatalities")))

(defn six-or-more-deaths?
  [incident-map]
  (if (and incident-map (<= 6 (get-fatalities incident-map)))
    incident-map))

(defn semiautomatic-weapon-in-use?
  [incident-map]
  (if (and incident-map (re-matches #".*(?:semi-?automatic|assault).*" (get-type-of-weapons incident-map)))
    incident-map))

(defn in-years-1984->2014?
  [incident-map]
  (if (and incident-map (<= 1984 (get-year incident-map) 2014))
    incident-map))

(defn get-decade-bucket
  [incident-map]
  (let [year (get-year incident-map)]
    (cond (<= 1984 year 1993)
          "1984-1994"

          (<= 1994 year 2003)
          "1994-2004"

          (<= 2004 year 2014)
          "2004-2014"

          :default
          (throw (Exception. "shouldn't happen")))))

(defn get-decade-bucket-summary
  [decade-bucket]
  (let [bucket (get-decade-bucket (first decade-bucket))
        deaths (apply + (map get-fatalities decade-bucket))
        incidents (count decade-bucket)]
    {:bucket bucket
     :incidents incidents
     :deaths deaths}))

(defn -main
  "print out bucket data for mass shootings from mother jones data set"
  [& args]
  (let [raw-map (raw->map raw)]
    (let [decade-buckets (->> (filter (comp in-years-1984->2014?         ; 40
                                            semiautomatic-weapon-in-use? ; 44
                                            six-or-more-deaths?)         ; 56
                                      raw-map)
                              (group-by get-decade-bucket))]
      (doseq [bucket-summary (map get-decade-bucket-summary (vals decade-buckets))]
        (prn bucket-summary)))))
