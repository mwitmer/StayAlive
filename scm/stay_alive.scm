(use-modules (stay-alive game)
	     (ncurses curses))

(define stdscr (initscr))
((the-game) 'play stdscr)
(endwin)
