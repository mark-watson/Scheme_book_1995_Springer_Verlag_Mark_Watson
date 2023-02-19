;; File: GO_TEST.S
 ;
 ; File module requirements: you must load GO_INIT.S and GO_DATA.S
 ;;


(load "go_init.s")
(load "go_data.s")

(define g (make-go-game 7))

(add-stone g COMPUTER 2 4)
(add-stone g COMPUTER 1 3)
(add-stone g COMPUTER 2 3)

(add-stone g HUMAN 2 2)
(add-stone g HUMAN 3 1)
(add-stone g HUMAN 2 1)

(update-liberties g 3)

(remove-group g 6)

(update-liberties g 3)

(touched-friend-stones g COMPUTER 2 3)

(group-attachment g 3)

(get-player-move)
(best-computer-move game)
