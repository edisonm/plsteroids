#!/bin/bash

set -e

PACKS="assertions rtchecks refactor xlibrary xtools"

forallpacks () {
    for pack in $PACKS; do
	echo "Note: On pack $pack"
	( cd $pack ; $* )
    done
}

case $* in
    tests)
	./plsteroids.sh -q -l autotester.pl -g 'ignore(run_tests),halt'
	;;
    cover)
	./plsteroids.sh -q -l autotester.pl -g 'ignore(autotester:cover_tests),browse_server(5000)'
	;;
    check)
	./plsteroids.sh -q -s loadall.pl -g 'checkall(_,[dir(pltool(prolog))])'
	;;
    *)
	forallpacks $*
	;;
esac
