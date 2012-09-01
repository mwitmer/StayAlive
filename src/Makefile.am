scmdir=$(datadir)/stay_alive

AM_CPPFLAGS = -DSCMDIR="\"$(scmdir)\""

bin_PROGRAMS = stay_alive

stay_alive_SOURCES = stay_alive.c
stay_alive_CFLAGS = @GUILE_CFLAGS@
stay_alive_LDADD = @GUILE_LIBS@
