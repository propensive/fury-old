VERSION=${shell sh -c 'cat .version 2> /dev/null || git --git-dir git/fury/.git describe --exact-match --tags 2> /dev/null || git --git-dir git/fury/.git rev-parse --short HEAD'}
BLOOPVERSION=1.3.2
BINDEPS=launcher ng.py ng
NAILGUNJAR=nailgun-server-1.0.0.jar
NAILGUNJARPATH=dist/bundle/lib/$(NAILGUNJAR)
NATIVEJARS=dist/bundle/lib/fury.jar $(NAILGUNJARPATH) bootstrap/scala/lib/scala-library.jar bootstrap/scala/lib/scala-reflect.jar
DOCKER_TAG=fury-ci
export PATH := $(PWD)/bootstrap/scala/bin:$(PATH)

all: dist/bundle/lib/fury.jar

publish: dist/install.sh
	gsutil -h "Cache-Control:public,max-age=60" cp $< gs://revivalist/downloads/fury.build/fury-$(VERSION).sh
	@echo
	@echo
	@echo "To install this version of Fury, run:"
	@echo
	@echo "  curl -OL https://storage.googleapis.com/revivalist/downloads/fury.build/fury-$(VERSION).sh"
	@echo "  sh install-$(VERSION).sh"
	@echo

dist/install.sh: dist/fury-$(VERSION).tar.gz dist/bundle/etc
	cat etc/install.sh $< > dist/install.sh
	LC_ALL=C sed -i.bak "s/FURY_VERSION=test/FURY_VERSION=$(VERSION)/" dist/install.sh
	chmod +x dist/install.sh

dist/fury-$(VERSION).tar.gz: dist/bundle/lib/fury.jar dist/bundle/bin/fury dist/bundle/etc
	tar czf $@ -C dist/bundle .

#TODO refactor etc structure (separate bundled scripts from development ones)
dist/bundle/etc:
	mkdir -p $@
	cp -r etc/aliases etc/bashrc etc/fishrc etc/zshrc etc/completion $@

# Compilation

bootstrap/scala:
	mkdir -p $@
	curl -s https://downloads.lightbend.com/scala/2.12.8/scala-2.12.8.tgz | tar xz -C $@ --strip 1

bootstrap/bin:
	mkdir -p $@

pre-compile: bootstrap/bin dist/bundle/bin/launcher bootstrap/scala $(NAILGUNJARPATH)

dist/bundle/bin/launcher: dist/bundle/bin/coursier dist/bundle/bin/.dir
	$< bootstrap --quiet -r bintray:scalacenter/releases -f --deterministic --output $@ ch.epfl.scala:bloop-launcher_2.12:$(BLOOPVERSION)

bootstrap/bin/fury/.version: bootstrap/bin/fury/.dir
	echo "$(VERSION)" > $@

# Libraries

dist/bundle/lib:
	mkdir -p $@

dist/bundle/lib/$(NAILGUNJAR): dist/bundle/lib
	curl -s -o $@ http://central.maven.org/maven2/com/facebook/nailgun-server/1.0.0/nailgun-server-1.0.0.jar

dist/bundle/lib/fury.jar: dist/bundle/lib bootstrap/bin bootstrap/bin/fury/.version
	fury build save -d $<
	mv $</fury-menu.jar $@
	#jar -uf $@ -C bootstrap/bin/fury .version

dist/bundle/lib/%.jar: bootstrap/bin bootstrap/bin/fury/.version dist/bundle/lib bootstrap/git/% compile
	jar -cf $@ -C $< $*

# Binaries

%/.dir:
	mkdir -p ${@D}
	touch ${@D}/.dir

dist/bundle/bin/fury: $(foreach D, $(BINDEPS), dist/bundle/bin/$(D))
	cp etc/fury $@
	chmod +x $@

dist/bundle/bin/coursier: dist/bundle/bin/.dir
	curl -s -L -o $@ https://github.com/coursier/coursier/releases/download/v1.1.0-M14-4/coursier
	chmod +x $@

dist/bundle/bin/ng.c: bootstrap/ng/.dir
	curl -s -L -o $@ https://raw.githubusercontent.com/facebook/nailgun/master/nailgun-client/c/ng.c

dist/bundle/bin/ng.py: dist/bundle/bin/.dir
	curl -s -L -o $@ https://raw.githubusercontent.com/facebook/nailgun/master/nailgun-client/py/ng.py
	sed -i.bak '1 s/$$/2.7/' $@ && rm $@.bak
	chmod +x $@

fury-native: dist/bundle/lib/fury.jar
	native-image -cp $(shell bash -c "ls $(NATIVEJARS) | paste -s -d: -") fury.Main

test:
	fury test

integration:
	etc/integration

test-isolated: ci
	@docker run -w /build -t $(DOCKER_TAG) make test

integration-isolated: ci
	@docker run -u root -w /root -t $(DOCKER_TAG) /bin/bash -c 'source ~/.bashrc; /integration'

docker-console: ci
	@docker run -u bash_user -w /home/bash_user -ti $(DOCKER_TAG) /bin/bash

ci:
	docker build -t $(DOCKER_TAG) .

clean-ci:
	docker build --no-cache -t fury-ci .

clean-dist:
	rm -rf dist

clean: clean-dist
	rm -rf bootstrap

download: dist/bundle/bin/coursier dist/bundle/bin/ng.py dist/bundle/bin/ng.c dist/bundle/bin/launcher dist/bundle/lib dist/bundle/lib/$(NAILGUNJAR) bootstrap/scala

install: dist/install.sh
	dist/install.sh

.PHONY: all publish pre-compile clean-dist clean test ci clean-ci test-isolated integration-isolated integration download install
