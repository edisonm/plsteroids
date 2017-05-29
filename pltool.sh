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
    extra_opt="-g assertz(package(`echo $to_load|sed -e 's:\/: :g'|awk '{print $2}'`)),[plsdirs]"
    run_tests="run_tests($2)"
elif [ "$#" == "3" ] ; then
    to_load=`find . -name $2.plt`
    extra_opt="-g assertz(package(`echo $to_load|sed -e 's:\/: :g'|awk '{print $2}'`)),[plsdirs]"
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
	swipl $extra_opt -g "['$to_load'],ignore($run_tests)" -t halt
	;;
    testst)
	swipl $extra_opt -g "['$to_load'],time($run_tests)" -t halt
	;;
    testrtc)
	swipl $extra_opt -g "['$to_load'],trace_rtc($run_tests)" -t halt
	;;
    teststrtc)
	swipl $extra_opt -g "['$to_load'],time(trace_rtc($run_tests))" -t halt
	;;
    cover)
        swipl -g "assertz(package(xtools))" $extra_opt \
              -g "[library(assertions)],[library(checkers)],['$to_load'],[library(gcover_unit),library(ws_cover)],browse_server(5000),ignore(cov_${run_tests}),cache_file_lines,www_open_url('http://localhost:5000')"
	;;
    check)
	if [ "$#" == "2" ] ; then
	    swipl -q -s loadall.pl -g "showcheck($2,[dir(pltool(prolog))])"
	else
	    swipl -q -s loadall.pl -g 'checkall([dir(pltool(prolog))])'
	fi
	;;
    checkt)
	if [ "$#" == "2" ] ; then
	    swipl -q -s loadall.pl -g "time(showcheck($2,[dir(pltool(prolog))]))"
	else
	    swipl -q -s loadall.pl -g 'time(checkall([dir(pltool(prolog))]))'
	fi
	;;
    checkload)
        if [ "$#" == "2" ] ; then
            swipl -q -s plsconfig.pl -g "assertz(package($2)),[checkload],halt"
        else
            for i in `find . -name pack.pl`; do
                pack=`basename ${i%/pack.pl}`
                echo "checking stand alone load of $pack"
                swipl -q -s plsconfig.pl -g "assertz(package($pack)),[checkload],halt"
            done
        fi
        ;;
    doc)
        swipl -s plsdoc.pl
        ;;
    checkc)
	if [ "$#" == "2" ] ; then
	    swipl -q -s loadall.pl -g "showcheck($2,[dir(pltool(prolog))])"
	else
	    swipl -q -s loadall.pl -g 'checkallc([dir(pltool(prolog))])'
	fi
	;;
    loadall)
	swipl -q -s loadall.pl
	;;
    build)
	echo -e "qsave_program(plsteroids,[]).\nhalt.\n" | \
	    swipl -q -s loadall.pl
	;;
    *)
	forallpacks $*
	;;
esac
