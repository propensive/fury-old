VERSION=${shell sh -c 'cat .version 2> /dev/null || git --git-dir git/fury/.git describe --exact-match --tags 2> /dev/null || git --git-dir git/fury/.git rev-parse --short HEAD'}
MKFILE := $(abspath $(lastword $(MAKEFILE_LIST)))
ROOTDIR := $(dir $(MKFILE))
BLOOPVERSION=1.2.5
DEPS=kaleidoscope totalitarian mitigation optometry eucalyptus exoskeleton escritoire mercator magnolia gastronomy contextual guillotine
REPOS:=$(foreach dep, $(DEPS), bootstrap/git/$(dep))
BINDEPS=launcher ng.py ng
NAILGUNJAR=nailgun-server-1.0.0.jar
NAILGUNJARPATH=dist/bundle/lib/$(NAILGUNJAR)
LIBS=bootstrap/scala/lib/scala-library.jar bootstrap/scala/lib/scala-reflect.jar
DEPENDENCY_JARS=$(foreach dep, $(DEPS), dist/bundle/lib/$(dep).jar)
JARS:= $(DEPENDENCY_JARS) dist/bundle/lib/fury.jar
NATIVEJARS=$(JARS) $(NAILGUNJARPATH) $(LIBS)
SRCS:=$(shell find $(PWD)/src -type f -name '*.scala')
CFGS:=$(shell ls etc/bloop)
DOCKER_TAG=fury-ci
TESTDEPS=ogdl core
TESTS=$(foreach DEP, $(TESTDEPS), $(DEP)-test)

all: all-jars

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
	echo "#!/usr/bin/env sh" > dist/install.sh
	echo "FURY_VERSION=$(VERSION)" >> dist/install.sh
	cat etc/install.sh $< >> dist/install.sh
	chmod +x dist/install.sh

dist/fury-$(VERSION).tar.gz: all-jars dist/bundle/bin/fury dist/bundle/etc
	tar czvf $@ -C dist/bundle .

#TODO refactor etc structure (separate bundled scripts from development ones)
dist/bundle/etc:
	mkdir -p $@
	cp -r etc/aliases etc/bashrc etc/fishrc etc/zshrc etc/completion etc/security $@

# Compilation

clean-compile: bloop-clean compile

watch: compile
	bloop compile fury/menu --watch

bloop-clean:
	bloop clean fury/menu

bootstrap/scala:
	mkdir -p $@
	curl https://downloads.lightbend.com/scala/2.12.8/scala-2.12.8.tgz | tar xvz -C $@ --strip 1

bootstrap/git/%: bootstrap/git/.dir
	mkdir -p $@
	git clone https://github.com/propensive/$*.git $@ --branch=fury

bootstrap/bin:
	mkdir -p $@

compile: dist/bundle/bin/launcher bootstrap/scala $(NAILGUNJARPATH) dependency-jars $(REPOS) $(SRCS) $(foreach CFG, $(CFGS), .bloop/$(CFG))
	$< --skip-bsp-connection $(BLOOPVERSION)
	bloop compile fury/menu

.bloop:
	mkdir -p .bloop

.bloop/%.json: .bloop
	sed "s#\$$ROOT#$(ROOTDIR)#" etc/bloop/$*.json > .bloop/$*.json

bootstrap/bin/fury/.version: bootstrap/bin/fury/.dir compile
	echo "$(VERSION)" > $@

# Libraries

dist/bundle/lib:
	mkdir -p $@

dist/bundle/lib/$(NAILGUNJAR): dist/bundle/lib/.dir
	curl -o $@ http://central.maven.org/maven2/com/facebook/nailgun-server/1.0.0/nailgun-server-1.0.0.jar

all-jars: $(JARS)

dist/bundle/lib/fury.jar: bootstrap/bin compile bootstrap/bin/fury/.version
	jar -cf $@ -C $< fury
	jar -uf $@ -C bootstrap/bin/fury .version

dist/bundle/lib/%.jar: bootstrap/bin bootstrap/bin/fury/.version dist/bundle/lib bootstrap/git/% compile
	jar -cf $@ -C $< $*

# Binaries

%/.dir:
	mkdir -p ${@D}
	touch $@

dist/bundle/bin/fury: $(foreach D, $(BINDEPS), dist/bundle/bin/$(D))
	cp etc/fury $@
	chmod +x $@

dist/bundle/bin/coursier: dist/bundle/bin/.dir
	curl -s -L -o $@ https://git.io/coursier
	chmod +x $@

jmh_jars=org.openjdk.jmh:jmh-core:1.21 org.openjdk.jmh:jmh-generator-bytecode:1.21 org.openjdk.jmh:jmh-generator-reflection:1.21 org.openjdk.jmh:jmh-generator-asm:1.21
bsp_jars=org.eclipse.lsp4j:org.eclipse.lsp4j.jsonrpc:0.6.0 ch.epfl.scala:bsp4j:2.0.0-M4
coursier_jars=io.get-coursier:coursier_2.12:1.1.0-M12
jtar_jars=org.kamranzafar:jtar:2.3
external_jars=$(jmh_jars) $(bsp_jars) $(coursier_jars) $(jtar_jars)

dependency-jars: dist/bundle/bin/coursier
	for JAR in $(shell dist/bundle/bin/coursier fetch $(external_jars)); do \
		cp $$JAR dist/bundle/lib/ ; \
	done

dist/bundle/bin/launcher: dist/bundle/bin/coursier dist/bundle/bin/.dir
	$< bootstrap --quiet -f --deterministic --output $@ ch.epfl.scala:bloop-launcher_2.12:$(BLOOPVERSION)

dist/bundle/bin/ng.c: bootstrap/ng/.dir
	curl -s -L -o $@ https://raw.githubusercontent.com/facebook/nailgun/master/nailgun-client/c/ng.c

dist/bundle/bin/ng.py: dist/bundle/bin/.dir
	curl -s -L -o $@ https://raw.githubusercontent.com/facebook/nailgun/master/nailgun-client/py/ng.py
	sed -i.bak '1 s/$$/2.7/' $@ && rm $@.bak
	chmod +x $@

fury-native: all-jars
	native-image -cp $(shell bash -c "ls $(NATIVEJARS) | paste -s -d: -") fury.Main

## Verification

$(TESTS): %-test: dist/bundle/bin/launcher bootstrap/git/probably
	$< --skip-bsp-connection $(BLOOPVERSION)
	bloop compile fury/$*-test
	bloop run fury/$*-test --main fury.Tests

test: bootstrap/bin/fury/.version $(TESTS)

integration:
	@echo "Not yet implemented"

test-isolated: ci
	@docker run -w /build -t $(DOCKER_TAG) make test

integration-isolated: ci
	@docker run -u bash_user -w /home/bash_user -t $(DOCKER_TAG) /test/run_all

ci:
	docker build -t $(DOCKER_TAG) .

clean-ci:
	docker build --no-cache -t fury-ci .

clean-dist:
	rm -rf dist

clean: clean-dist
	rm -rf bootstrap/bin/fury
	rm -rf bootstrap
	rm -rf .bloop

download: $(REPOS) dist/bundle/bin/coursier dist/bundle/bin/ng.py dist/bundle/bin/ng.c dist/bundle/bin/launcher dist/bundle/lib/$(NAILGUNJAR) bootstrap/scala
	dist/bundle/bin/launcher --skip-bsp-connection $(BLOOPVERSION) # to download bloop

install: dist/install.sh
	dist/install.sh

.PHONY: all publish compile watch bloop-clean clean-compile clean-dist clean test ci clean-ci test-isolated integration-isolated integration $(TESTS) all-jars download install dependency-jars
