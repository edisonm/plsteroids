#!/bin/bash

set -e

PACKS="assertions rtchecks refactor xlibrary xtools"

forallpacks () {
    for pack in $PACKS; do
	echo "Note: On pack $pack"
	( cd $pack ; $* )
    done
}

if [ "$#" == "2" ] ; then
    to_load=`find . -name $2.plt`
    run_tests="run_tests($2)"
elif [ "$#" == "3" ] ; then
    to_load=`find . -name $2.plt`
    run_tests="run_tests($2:$3)"
else
    to_load="autotester.pl"
    run_tests="run_tests"
fi

case $1 in
    patches)
	forallpacks git format-patch origin
	find . -name "*.patch"|tar -cvzf patches.tgz -T -
	find . -name "*.patch" -delete
	;;
    tests)
	./plsteroids.sh -l $to_load -g "ignore($run_tests)" -t halt
	;;
    testst)
	./plsteroids.sh -l $to_load -g "time($run_tests)" -t halt
	;;
    testrtc)
	./plsteroids.sh -q -l $to_load -g "trace_rtc($run_tests)" -t halt
	;;
    teststrtc)
	./plsteroids.sh -l $to_load -g "time(trace_rtc($run_tests))" -t halt
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
	if [ "$#" == "2" ] ; then
	    ./plsteroids.sh -q -s loadall.pl -g "showcheck($2,[dir(pltool(prolog))])"
	else
	    ./plsteroids.sh -q -s loadall.pl -g 'checkallc([dir(pltool(prolog))])'
	fi
	;;
    loadall)
	./plsteroids.sh -q -s loadall.pl
	;;
    build)
	echo -e "qsave_program(plsteroids,[]).\nhalt.\n" | \
	    ./plsteroids.sh -q -s loadall.pl
	;;
    *)
	forallpacks $*
	;;
esac
