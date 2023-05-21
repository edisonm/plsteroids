#!/bin/bash

set -e

PACKS="assertions rtchecks stchecks refactor xlibrary xtools"

forallpacks () {
    for pack in $PACKS; do
	echo "Note: On pack $pack"
	( cd $pack ; $* )
    done
}

if [ "$#" == "2" ] ; then
    to_load=`find . -name $2.plt`
    extra_opt=" -g assertz(package(stchecks)) -g assertz(package(`echo $to_load|sed -e 's:\/: :g'|awk '{print $2}'`)),[plsdirs,library(assertions),library(stchecks)]"
    run_tests="run_tests($2)"
elif [ "$#" == "3" ] ; then
    to_load=`find . -name $2.plt`
    extra_opt=" -g assertz(package(stchecks)) -g assertz(package(`echo $to_load|sed -e 's:\/: :g'|awk '{print $2}'`)),[plsdirs,library(assertions),library(stchecks)]"
    run_tests="run_tests($2:$3)"
else
    to_load="autotester.pl"
    run_tests="run_tests"
fi

case $1 in
    tests)
	swipl -tty $extra_opt -g "['$to_load'],time($run_tests)" -t halt
	;;
    testrtc)
	swipl -tty -g "assertz(package(rtchecks))" $extra_opt -g "['$to_load'],[library(rtchecks_utils)],time(with_rtchecks($run_tests))" -t halt
	;;
    cover)
        swipl -tty $extra_opt \
              -g "['$to_load'],[library(gcover_unit),library(ws_cover)],browse_server(5000),time((cov_${run_tests},cache_file_lines)),www_open_url('http://localhost:5000')"
	;;
    *)
	forallpacks $*
	;;
esac
