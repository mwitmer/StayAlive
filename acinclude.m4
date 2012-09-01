## Code taken from guile.m4

## Autoconf macros for working with Guile.
##
##   Copyright (C) 1998,2001, 2006. 2010 Free Software Foundation, Inc.
##
## This library is free software; you can redistribute it and/or
## modify it under the terms of the GNU Lesser General Public License
## as published by the Free Software Foundation; either version 3 of
## the License, or (at your option) any later version.
## 
## This library is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## Lesser General Public License for more details.
## 
## You should have received a copy of the GNU Lesser General Public
## License along with this library; if not, write to the Free Software
## Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
## 02110-1301 USA

AC_DEFUN([GUILE_PROGS],
 [AC_PATH_PROG(GUILE,guile)
  if test "$GUILE" = "" ; then
      AC_MSG_ERROR([guile required but not found])
  fi
  AC_SUBST(GUILE)
  AC_PATH_PROG(GUILE_CONFIG,guile-config)
  if test "$GUILE_CONFIG" = "" ; then
      AC_MSG_ERROR([guile-config required but not found])
  fi
  AC_SUBST(GUILE_CONFIG)
  AC_PATH_PROG(GUILE_TOOLS,guile-tools)
  AC_SUBST(GUILE_TOOLS)
 ])

AC_DEFUN([GUILE_FLAGS],
 [AC_REQUIRE([GUILE_PROGS])dnl
  AC_MSG_CHECKING([libguile compile flags])
  GUILE_CFLAGS="`$GUILE_CONFIG compile`"
  AC_MSG_RESULT([$GUILE_CFLAGS])
  AC_MSG_CHECKING([libguile link flags])
  GUILE_LDFLAGS="`$GUILE_CONFIG link`"
  AC_MSG_RESULT([$GUILE_LDFLAGS])
  AC_SUBST(GUILE_CFLAGS)
  AC_SUBST(GUILE_LDFLAGS)
 ])

AC_DEFUN([GUILE_MODULE_CHECK],
         [AC_MSG_CHECKING([if $2 $4])
	  GUILE_CHECK($1,(use-modules $2) (exit ((lambda () $3))))
	  if test "$$1" = "0" ; then $1=yes ; else $1=no ; fi
          AC_MSG_RESULT($$1)
         ])
	 
AC_DEFUN([GUILE_CHECK],
 [AC_REQUIRE([GUILE_PROGS])
  $GUILE -c "$2" > /dev/null 2>&1
  $1=$?
 ])

AC_DEFUN([GUILE_MODULE_REQUIRED],
         [GUILE_MODULE_AVAILABLE(ac_guile_module_required, ($1))
          if test "$ac_guile_module_required" = "no" ; then
              AC_MSG_ERROR([required guile module not found: ($1)])
          fi
         ])

AC_DEFUN([GUILE_MODULE_AVAILABLE],
         [GUILE_MODULE_CHECK($1,$2,0,is available)
         ])

AC_DEFUN([GUILE_MODULE_CHECK],
         [AC_MSG_CHECKING([if $2 $4])
	  GUILE_CHECK($1,(use-modules $2) (exit ((lambda () $3))))
	  if test "$$1" = "0" ; then $1=yes ; else $1=no ; fi
          AC_MSG_RESULT($$1)
         ])

AC_DEFUN([GUILE_CHECK],
 [AC_REQUIRE([GUILE_PROGS])
  $GUILE -c "$2" > /dev/null 2>&1
  $1=$?
 ])

AC_DEFUN([GUILE_SITE_DIR],
 [AC_REQUIRE([GUILE_PROGS])dnl
  AC_MSG_CHECKING(for Guile site directory)
  GUILE_SITE=`[$GUILE_CONFIG] info sitedir`
  if test "$GUILE_SITE" = ""; then
     GUILE_SITE=`[$GUILE_CONFIG] info pkgdatadir`/site
  fi
  AC_MSG_RESULT($GUILE_SITE)
  AC_SUBST(GUILE_SITE)
 ])
