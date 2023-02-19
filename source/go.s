;; File: GO.S
 ;
 ; Description: this file loads all the other Go program files.
 ;
 ;;

(load "GO_INIT.S")
(load "GO_DATA.S")
(load "GO_PLAY.S")
(load "GO_PLOT.S")
(load "GO_TACT.S")
(load "GO_STRAT.S")

(newline)
(display "Type '(go)' to start a game.")
(newline)
(newline)
