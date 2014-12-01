(ns logol.core
  (:require [clojure.string :as s]))

;;; The goal of this library being to find patterns like these in literature
;;; and other instances of text (especially records of unscripted spoken dialogue) 


(defn strings->regex [strings]
  (re-pattern (s/join  (interpose "|" strings))))

;;; if its false return which letters were found? return empty if none were?
(defn lipogram? [string omitions]
  "Returns true if the string contains none of the characters in omitions"
  (empty? (re-find (strings->regex omitions) string)))

;;; returns true if the word contains all letters and if not returns which
;;; its missing. An odd way to handle booleans since it'll always be true
;;; use to see words containing all five vowels.
(defn word-contains? [string letters]
  "Returns true if word contains them all and false if not"
  (empty?
   (filter nil? (map re-find (map re-pattern letters)
                     (repeat (count letters) string)))))

;;; checks to see if the beginning of every word in the string is the same
;;; an acrostic fits a scheme, not necessarily this one. Could spell out a
;;; name, follow a different lettering scheme (like months of the year)
;;; assumes that you're not giving a series of lines.
;;; represent as CFG?
(defn acrostic? [string]
  (every? #{(first (s/lower-case string))}
          (map first (s/split (s/lower-case string) #" "))))

(defn acrostic-scheme? [string scheme]
  ;; the problem is schemes could be anything, a sequence of letters, one that should be repeated for every word, a scheme to be determined by the first word.
  )
