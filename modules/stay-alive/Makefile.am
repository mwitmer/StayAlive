moduledir=$(GUILE_SITE)/stay-alive
module_DATA = extensions.scm lang.scm ncurses-interface.scm shared.scm util.scm weight.scm \
		agent.scm armor.scm body.scm delta-queue.scm dungeon.scm game.scm item.scm level.scm \
		 player.scm square.scm	timer.scm weapon.scm

extensions.scm: extensions.scm.in
	@SED@ 's|XXextensionsdirXX|$(libdir)/stay_alive|' <extensions.scm.in >extensions.scm