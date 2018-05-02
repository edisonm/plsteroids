# PACKS=assertions rtchecks refactor xlibrary xtools
SHELL=/bin/bash
MAKEFLAGS += --silent --no-print-directory
# JOBS?=$(shell nproc)

PLSTEROIDS=target/bin/plsteroids

CONCURRENT=. bin/concurrent ; run_pull

build: $(PLSTEROIDS)
	true

clean:
	$(RM) -rf target

$(PLSTEROIDS):
	mkdir -p `dirname $@`
	echo -e "infer_meta_if_required.\nqsave_program('$@',[]).\nhalt.\n" | swipl -q -s loadall.pl

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

stests: $(addsuffix .stest,$(CHECKERS))
	true

%.stest: noop
	$(CONCURRENT) bin/run_stest '$@'

tests:
	$(MAKE) stests utests

get_plsteroids:
	echo $(PLSTEROIDS)
