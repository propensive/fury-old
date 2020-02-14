VERSION=${shell sh -c 'cat .version 2> /dev/null || git --git-dir git/fury/.git describe --exact-match --tags 2> /dev/null || git --git-dir git/fury/.git rev-parse --short HEAD'}
FURYSTABLE=0.8.2
FURYLOCAL=opt/fury-$(FURYSTABLE)/bin/fury
BINDEPS=coursier ng.py ng
NAILGUNJAR=nailgun-server-1.0.0.jar
NAILGUNJARPATH=dist/bundle/lib/$(NAILGUNJAR)
NATIVEJARS=dist/bundle/lib/fury-frontend.jar $(NAILGUNJARPATH) bootstrap/scala/lib/scala-library.jar bootstrap/scala/lib/scala-reflect.jar
DOCKER_TAG=fury-ci
INIT_CGROUP=$(shell cat /proc/1/cgroup 2> /dev/null || echo '' | tail -n1 | cut -d: -f3)
ifeq ($(INIT_CGROUP),"/")
	FURY_OUTPUT=graph
else
	FURY_OUTPUT=linear
endif

export PATH := $(PWD)/bootstrap/scala/bin:$(PATH)

all: dist/bundle/lib/fury-frontend.jar

publish-ipfs: dist/fury-$(VERSION).tar.gz
	@ipfs add dist/fury-$(VERSION).tar.gz

watch:
	@echo "Run,"
	@echo
	@echo "    etc/react <test name>"
	@echo
	@echo "in another terminal to run an integration test upon a successful build"
	@echo
	fury build save -w -d dist/bundle/lib --output linear

publish: dist/install.sh
	git tag "v$(VERSION)" -m "Version $(VERSION)"
	gsutil -h "Cache-Control:public,max-age=60" cp $< gs://downloads.furore.dev/fury-$(VERSION).sh
	git push --tags
	@echo
	@echo "To install this version of Fury, run:"
	@echo
	@echo "  curl -OL http://downloads.furore.dev/fury-$(VERSION).sh"
	@echo "  sh install-$(VERSION).sh"
	@echo

opt:
	mkdir -p opt

dist/bundle/bin/procname.c:
	cp etc/procname.c $@

opt/fury-$(FURYSTABLE).sh: opt
	curl -C - -s -o $@ "http://downloads.furore.dev/fury-$(FURYSTABLE).sh"

$(FURYLOCAL): opt/fury-$(FURYSTABLE).sh
	sh opt/fury-$(FURYSTABLE).sh opt/fury-$(FURYSTABLE)
	touch $(FURYLOCAL)

dist/install.sh: dist/fury-$(VERSION).tar.gz dist/bundle/etc
	cat etc/install.sh $< > dist/install.sh
	LC_ALL=C sed -i.bak "s/FURY_VERSION=test/FURY_VERSION=$(VERSION)/" dist/install.sh && rm dist/install.sh.bak
	chmod +x dist/install.sh

dist/fury-$(VERSION).tar.gz: dist/bundle/lib/fury-frontend.jar dist/bundle/bin/fury dist/bundle/etc dist/bundle/bin/upgrade
	cp .version dist/bundle/
	tar czf $@ -C dist/bundle . 2> /dev/null

#TODO refactor etc structure (separate bundled scripts from development ones)
dist/bundle/etc:
	mkdir -p $@
	cp -r etc/aliases etc/bashrc etc/fishrc etc/zshrc etc/completion $@

# Compilation

bootstrap/scala:
	mkdir -p $@
	curl -s https://downloads.lightbend.com/scala/2.12.8/scala-2.12.8.tgz | tar xz -C $@ --strip 1 2> /dev/null

bootstrap/bin:
	mkdir -p $@

bootstrap/build.fury: bootstrap/bin
	cp -n .focus.fury .fury.conf 2> /dev/null | true
	tar cvzf $@ .focus.fury .fury.conf layers/*

pre-compile: bootstrap/bin bootstrap/scala $(NAILGUNJARPATH)

# Libraries

dist/bundle/lib:
	mkdir -p $@

dist/bundle/lib/$(NAILGUNJAR): dist/bundle/lib
	curl -s -o $@ http://central.maven.org/maven2/com/facebook/nailgun-server/1.0.0/nailgun-server-1.0.0.jar

dist/bundle/lib/fury-frontend.jar: dist/bundle/lib $(FURYLOCAL) bootstrap/build.fury bootstrap/bin .version src/**/*.scala
	PATH=$(PATH):opt/fury-$(FURYSTABLE)/bin $(FURYLOCAL) layer extract -f bootstrap/build.fury
	$(FURYLOCAL) permission grant --module frontend --project fury -P 729
	$(FURYLOCAL) layer select -l /platform/jawn
	$(FURYLOCAL) permission grant --module ast --project jawn -P b7a
	$(FURYLOCAL) layer select -l /
	$(FURYLOCAL) build save --https --output $(FURY_OUTPUT) --project fury --module frontend --dir $<
	jar -uf $@ .version

dist/bundle/lib/%.jar: bootstrap/bin .version dist/bundle/lib bootstrap/git/% compile
	jar -cf $@ -C $< $*

# Binaries

%/.dir:
	mkdir -p ${@D}
	touch ${@D}/.dir

dist/bundle/bin/fury: dist/bundle/bin/.dir dist/bundle/bin/ng.c dist/bundle/bin/procname.c dist/bundle/bin/coursier
	cp etc/fury $@
	chmod +x $@

dist/bundle/bin/upgrade: dist/bundle/bin/.dir
	cp etc/upgrade $@
	chmod +x $@

dist/bundle/bin/coursier: dist/bundle/bin/.dir
	curl -s -L -o $@ https://github.com/coursier/coursier/releases/download/v2.0.0-RC5-3/coursier
	chmod +x $@

dist/bundle/bin/ng.c:
	curl -s -L -o $@ https://raw.githubusercontent.com/facebook/nailgun/master/nailgun-client/c/ng.c

dist/bundle/bin/ng.py: dist/bundle/bin/.dir
	curl -s -L -o $@ https://raw.githubusercontent.com/facebook/nailgun/master/nailgun-client/py/ng.py
	sed -i.bak '1 s/$$/2.7/' $@ && rm $@.bak
	chmod +x $@

fury-native: dist/bundle/lib/fury-frontend.jar
	native-image -cp $(shell bash -c "ls $(NATIVEJARS) | paste -s -d: -") fury.Main

test: test-setup test-strings test-ogdl test-model test-core test-io

test-setup: bootstrap/build.fury
	fury layer extract -f $<
	fury permission grant --project fury --module test-core -P 228 4a8 538 7f0 c0d c2e f90 00b b7a
	fury permission grant --project fury --module test-io -P aa7

test-%: test-setup
	fury build run --https --output $(FURY_OUTPUT) --project fury --module $@

integration:
	etc/integration

community:
	etc/community

test-isolated: ci
	@docker run -f etc/docker/build -w /build -t $(DOCKER_TAG) make test

integration-isolated: ci
	@docker run -f etc/docker/build -u bash_user -w /home/bash_user -t $(DOCKER_TAG) /bin/bash -c 'source ~/.bashrc; /integration'

community-isolated: ci
	@docker run -f etc/docker/build -u bash_user -w /home/bash_user -t $(DOCKER_TAG) /bin/bash -c 'source ~/.bashrc; /community'

docker-console: ci
	@docker run -f etc/docker/build -u bash_user -w /home/bash_user -ti $(DOCKER_TAG) /bin/bash

ci:
	docker build -f etc/docker/build -t $(DOCKER_TAG) .

clean-ci:
	docker build -f etc/docker/build --no-cache -t fury-ci .

clean: clean-dist
	rm -rf bootstrap dist opt

download: dist/bundle/bin/coursier dist/bundle/bin/ng.py dist/bundle/bin/ng.c dist/bundle/lib dist/bundle/lib/$(NAILGUNJAR) bootstrap/scala

install: dist/install.sh
	dist/install.sh

revise:
	etc/revise

.PHONY: all bootstrap/build.fury publish pre-compile clean-dist clean test ci clean-ci test-isolated test-% integration-isolated integration community-isolated community download install revise
