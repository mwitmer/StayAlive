#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_INIT(stay_alive, 0.1, mark.d.witmer@gmail.com)

AC_CONFIG_AUX_DIR([build_aux])
AM_INIT_AUTOMAKE([gnu std-options -Wall])

AC_CONFIG_MACRO_DIR([m4])
AC_PREREQ([2.68])

AC_CONFIG_SRCDIR([src/stay_alive.c])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([
	Makefile
	src/Makefile
	lib/Makefile
	scm/Makefile
	modules/stay-alive/Makefile
])

LT_INIT

pkgdatadir="$datadir/$PACKAGE"

# Checks for programs.
AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_SED

PKG_CHECK_MODULES([GUILE], [guile-2.0])
GUILE_SITE_DIR
GUILE_MODULE_REQUIRED([shelf shelf])
GUILE_MODULE_REQUIRED([ncurses curses])

# Checks for libraries.

# Checks for header files.
AC_CHECK_HEADERS([float.h stddef.h stdlib.h string.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_STDBOOL
AC_TYPE_SIZE_T
AC_TYPE_SSIZE_T

# Checks for library functions.
AC_FUNC_MALLOC

AC_OUTPUT
