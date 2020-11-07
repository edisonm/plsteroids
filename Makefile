# PACKS=assertions rtchecks refactor xlibrary xtools
SHELL=/bin/bash
MAKEFLAGS += --silent --no-print-directory
# JOBS?=$(shell nproc)

PLSTEROIDS=target/bin/plsteroids

CONCURRENT=. bin/concurrent ; concurrent_silent=1 ; run_pull

build:
	cd idfpml/prolog/idfpml/LIBRARY ; \
	  $(MAKE) CC=gcc CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0
	$(MAKE) $(PLSTEROIDS)
	true

compile:
	for i in `find . -name pack.pl`; do \
	    pack=`basename $${i%/pack.pl}` ; \
	    find $${pack}/prolog -type d -exec mkdir -p target/lib/{} \; ; \
	    find $${pack}/prolog -type f -name "*.pl" -exec ln -sf `pwd`/{} target/lib/{} \; ; \
	done
	swipl -q -s qcompile.pl -t halt

clean:
	cd idfpml/prolog/idfpml/LIBRARY ; $(MAKE) clean
	$(RM) -rf target assertions/tests/foreign/*{.o,.so,_so.pl,_impl.h,_intf.c,_intf.h} \
		idfpml/prolog/*{.o,.so,_so.pl,_impl.h,_intf.c,_intf.h,_auto.*}
	mkdir -p target/bin

$(PLSTEROIDS):
	mkdir -p `dirname $@`
	echo -e "infer_meta:infer_meta_if_required.\nqsave_program('$@',[]).\nhalt.\n" | swipl -q -s loadall.pl

patches:
	forallpacks git format-patch origin
	find . -name "*.patch"|tar -cvzf patches.tgz -T -
	find . -name "*.patch" -delete

push:
	git push
	for i in `git subrepo status -q` ; do \
	  git subrepo push $${i} ; \
	done
	git push

pull:
	git pull
	for i in `git subrepo status -q` ; do \
	  git subrepo pull $${i} ; \
	done

noop:

%.plt: noop
	$(CONCURRENT) bin/run_utest '$@'

%.plr: noop
	$(CONCURRENT) bin/run_rtest '$@'


%.utest:
	$(MAKE) `find $* -name "*.plt"`

%.rtest:
	$(MAKE) $(shell for i in `find $* -name '*.plt'`; do echo $${i%.*}.plr; done)

CHECKERS=$(shell for i in `find . -name "check_*.pl"`; do echo `basename $${i%.*}`|sed -e s:check_::g ; done)

utests: $(shell find * -name "*.plt")
	true

rtests: $(shell for i in `find $* -name '*.plt'`; do echo $${i%.*}.plr; done)
	true

stests: $(addsuffix .stest,$(CHECKERS))
	true

%.stest: noop
	$(CONCURRENT) bin/run_stest '$@'

tests:
	$(MAKE) stests utests

get_plsteroids:
	echo $(PLSTEROIDS)

doc:
	swipl -s plsdoc.pl
