AC_INIT(
	[Energy Based Evolutionary Algorithm] ,
	[1.0] ,
	[mariano.pedro@gmail.com] ,
	[ebea-1.0] ,
	[https://github.com/plsm/ebea] )

############################################################
# check for mercury
AC_PATH_PROG(MMC,mmc,notFound)
if test "$MMC" = notFound ; then
	AC_MSG_ERROR(["Mercury compiler is required!"])
fi

############################################################
# check for mc4ap
AS_ECHO_N("checking for mc4ap...")
AC_ARG_VAR(lib_mc4ap, [path where mc4ap components (ui_swing or libui_console) were installed])
if test -z "$lib_mc4ap" ; then
	lib_mc4ap=$(AS_DIRNAME($MMC))
	lib_mc4ap=$(AS_DIRNAME($lib_mc4ap))
fi
if test -n "$lib_mc4ap" && test -d "$lib_mc4ap" ; then
	AS_ECHO_N(" in $lib_mc4ap...")
	if test -d $lib_mc4ap/lib/mercury/lib && test -d $lib_mc4ap/lib/mercury/ints ; then
		PLACES1=$(find $lib_mc4ap/lib/mercury/lib -maxdepth 2 -name 'libui_console.so' -o -name 'ui_swing.jar')
		PLACES2=$(find $lib_mc4ap/lib/mercury/ints -maxdepth 1 -name 'userInterface.int')
		if test -n "$PLACES1" && test -n "$PLACES2" ; then
			AS_ECHO(" found")
		else
			AS_ECHO("")
			AC_MSG_ERROR([Could not find any mc4ap libraries])
		fi
	else
		AS_ECHO("")
		AC_MSG_ERROR([Directories $lib_mc4ap/lib/mercury/lib and $lib_mc4ap/lib/mercury/ints do not exist])
	fi
else
	AC_MSG_ERROR([$lib_mc4ap is not a valid directory])
fi

############################################################
# check for mercury_utils
AS_ECHO_N("checking for mercury_utils...")
AC_ARG_VAR(lib_mercury_utils, [path where mercury_utils components (mercury_utils.jar or libmercury_utils.so) were installed])
if test -z "$lib_mercury_utils" ; then
	lib_mercury_utils=$(AS_DIRNAME($MMC))
	lib_mercury_utils=$(AS_DIRNAME($lib_mercury_utils))
fi
if test -n "$lib_mercury_utils" && test -d "$lib_mercury_utils" ; then
	AS_ECHO_N(" in $lib_mercury_utils...")
	if test -d $lib_mercury_utils/lib/mercury/lib && test -d $lib_mercury_utils/lib/mercury/ints ; then
		PLACES1=$(find $lib_mercury_utils/lib/mercury/lib -maxdepth 2 -name 'libmercury_utils.so' -o -name 'mercury_utils.jar')
		PLACES2=$(find $lib_mercury_utils/lib/mercury/ints -maxdepth 1 -name 'mercury_utils.int')
		if test -n "$PLACES1" && test -n "$PLACES2" ; then
			AS_ECHO(" found")
#			AS_ECHO("$PLACES1")
#			AS_ECHO("$PLACES2")
		else
			AS_ECHO("")
			AC_MSG_ERROR([Could not find any mercury_utils libraries])
		fi
	else
		AS_ECHO("")
		AC_MSG_ERROR([Directories $lib_mercury_utils/lib/mercury/lib and $lib_mercury_utils/lib/mercury/ints do not exist])
	fi
else
	AC_MSG_ERROR([$lib_mercury_utils is not a valid directory])
fi


############################################################
# check for java 
AC_CHECK_PROG(JAVAC,javac,found)
if test "$JAVAC" = found ; then
	# see if mercury has the java grade
	mercury_java_lib=$(AS_DIRNAME($MMC))
	mercury_java_lib=$(AS_DIRNAME($mercury_java_lib))/lib/mercury/lib/java
	if test -d $mercury_java_lib ; then
		echo "Mercury has the java grade."
		mercury_java=$mercury_java_lib/mer_std.jar:$mercury_java_lib/mer_rt.jar
		use_swing=yes
	else
		use_swing=no
	fi
else
	use_swing=no
fi

AC_SUBST(use_swing)
AC_SUBST(mercury_java)

AC_CONFIG_FILES(Makefile)

AC_OUTPUT
