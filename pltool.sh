#!/bin/bash

set -e

PACKS="assertions rtchecks refactor xlibrary xtools"

forallpacks () {
    for pack in $PACKS; do
	echo "Note: On pack $pack"
	( cd $pack ; $* )
    done
}

case $1 in
    tests)
	./plsteroids.sh -l autotester.pl -g 'ignore(run_tests),halt'
	;;
    rtc)
	./plsteroids.sh -q -l autotester.pl -g 'trace_rtc(run_tests),halt'
	;;
    cover)
	./plsteroids.sh -q -l autotester.pl -g 'ignore(autotester:cover_tests),browse_server(5000)'
	;;
    check)
	./plsteroids.sh -q -s loadall.pl -g 'checkall([dir(pltool(prolog))])'
	;;
    checkc)
	./plsteroids.sh -q -s loadall.pl -g 'checkallc([dir(pltool(prolog))])'
	;;
    loadall)
	./plsteroids.sh -q -s loadall.pl
	;;
    build)
	echo -e "qsave_program(plsteroids,[]).\nhalt.\n" | \
	    ./plsteroids.sh -q -s loadall.pl
	;;
    test)
	./plsteroids.sh -l $2 -g 'ignore(run_tests),halt'
	;;
    *)
	forallpacks $*
	;;
esac
