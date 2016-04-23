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
    patches)
	forallpacks git format-patch origin
	find . -name "*.patch"|tar -cvzf patches.tgz -T -
	find . -name "*.patch" -delete
	;;
    tests)
	./plsteroids.sh -l autotester.pl -g 'run_tests' -t halt
	;;
    testst)
	./plsteroids.sh -l autotester.pl -g 'time(run_tests)' -t halt
	;;
    rtc)
	./plsteroids.sh -q -l autotester.pl -g 'trace_rtc(run_tests)' -t halt
	;;
    cover)
	./plsteroids.sh -q -l autotester.pl -g 'ignore(autotester:cover_tests),browse_server(5000)'
	;;
    check)
	if [ "$#" == "2" ] ; then
	    ./plsteroids.sh -q -s loadall.pl -g "showcheck($2,[dir(pltool(prolog))])"
	else
	    ./plsteroids.sh -q -s loadall.pl -g 'checkall([dir(pltool(prolog))])'
	fi
	;;
    checkt)
	if [ "$#" == "2" ] ; then
	    ./plsteroids.sh -q -s loadall.pl -g "time(showcheck($2,[dir(pltool(prolog))]))"
	else
	    ./plsteroids.sh -q -s loadall.pl -g 'time(checkall([dir(pltool(prolog))]))'
	fi
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
	./plsteroids.sh -l $2 -g 'ignore(run_tests)' -t halt
	;;
    testt)
	./plsteroids.sh -l $2 -g 'time(run_tests)' -t halt
	;;
    testrtc)
	./plsteroids.sh -l $2 -g 'trace_rtc(run_tests)' -t halt
	;;
    testrtct)
	./plsteroids.sh -l $2 -g 'time(trace_rtc(run_tests))' -t halt
	;;
    *)
	forallpacks $*
	;;
esac
