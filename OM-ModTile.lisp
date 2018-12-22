;;;===================================================
;;;
;;; OM-ModTile
;;; 
;;; Hélianthe Caure (IRCAM - 2016)
;;;
;;;===================================================

(in-package :om)

(compile&load (namestring (om-relative-path '("sources") "automaton-tiling")))
(compile&load (namestring (om-relative-path '("sources") "automaton-tiling-editor")))
(compile&load (namestring (om-relative-path '("sources") "tiling-mod-2")))

;;;Use HTML in doc (<br>, <a> etc)
(doc-library "OM-ModTile..." (find-library "OM-ModTile"))

(om::fill-library '((nil (("Tiling Modulo 2"
                           nil 
                           (automaton-tiling)
                           (find_rhythm_mod_2
                            make_mod_2_canon
                            is_vuza_p
                            zoom-canon
                            is_transferable_p
                            transfert-transform)
                           nil)) nil nil nil))
                         
                  (om::find-library "OM-ModTile"))

(om::set-lib-release 0.2)

(doc-library
"<br><br>
This library allows to create, modify, visualize and experiment with modulo 2 compact rhythmic canons. Every completion is based on a greedy algorithm, and given an inner rhythm creates the smaller outer rhythm to obtain a mod 2 compact canon. Rhythms are represented by lists of integers symbolizing the onsets.
<br><br>
For practical examples, see <a target=\"_blank\" href=\"https://hal.archives-ouvertes.fr/hal-01161082\"> here</a>.
"
(find-library "OM-ModTile"))

(om-print "
;;;===================================
;;; OM-ModTile
;;; (c) H. Caure - IRCAM - Representations Musicales - 2016
;;;===================================
")