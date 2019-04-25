VERSION=${shell sh -c 'cat .version 2> /dev/null || git --git-dir git/fury/.git describe --exact-match --tags 2> /dev/null || git --git-dir git/fury/.git rev-parse --short HEAD'}
MKFILE := $(abspath $(lastword $(MAKEFILE_LIST)))
ROOTDIR := $(dir $(MKFILE))
BLOOPVERSION=1.2.5
DEPS=kaleidoscope optometry eucalyptus exoskeleton escritoire mercator magnolia gastronomy contextual guillotine
REPOS:=$(foreach dep, $(DEPS), bootstrap/git/$(dep))
BINDEPS=launcher ng.py ng
NAILGUNJAR=nailgun-server-1.0.0.jar
NAILGUNJARPATH=dist/bundle/lib/$(NAILGUNJAR)
LIBS=bootstrap/scala/lib/scala-library.jar bootstrap/scala/lib/scala-reflect.jar
DEPENDENCY_JARS=$(foreach dep, $(DEPS), dist/bundle/lib/$(dep).jar)
JARS:= $(DEPENDENCY_JARS) dist/bundle/lib/fury.jar
NATIVEJARS=$(JARS) $(NAILGUNJARPATH) $(LIBS)
SRCS:=$(shell find $(PWD)/src -type f -name '*.scala')
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

watch: bootstrap/bin/fury
	bloop compile fury/menu --watch

bloop-clean:
	bloop clean fury/menu

bootstrap/scala: bootstrap/bin
	mkdir -p $@
	curl https://downloads.lightbend.com/scala/2.12.8/scala-2.12.8.tgz | tar xvz -C $@ --strip 1
	touch bootstrap/scala

bootstrap/git/%:
	mkdir -p $@
	git clone https://github.com/propensive/$*.git $@ --branch=fury

bootstrap/bin:
	mkdir -p $@

bootstrap/bin/contextual: bootstrap/scala bootstrap/git/contextual
	bootstrap/scala/bin/scalac -d bootstrap/bin -cp bootstrap/bin bootstrap/git/contextual/src/core/*.scala
	touch bootstrap/bin/contextual

bootstrap/bin/mercator: bootstrap/scala bootstrap/git/mercator
	bootstrap/scala/bin/scalac -d bootstrap/bin -cp bootstrap/bin bootstrap/git/mercator/src/core/*.scala
	touch bootstrap/bin/mercator

bootstrap/bin/magnolia: bootstrap/scala bootstrap/git/magnolia bootstrap/bin/mercator
	bootstrap/scala/bin/scalac -d bootstrap/bin -cp bootstrap/bin bootstrap/git/magnolia/src/core/*.scala
	touch bootstrap/bin/magnolia

bootstrap/bin/guillotine: bootstrap/scala bootstrap/git/guillotine bootstrap/bin/contextual
	bootstrap/scala/bin/scalac -d bootstrap/bin -cp bootstrap/bin bootstrap/git/guillotine/src/macros/*.scala
	bootstrap/scala/bin/scalac -d bootstrap/bin -cp bootstrap/bin bootstrap/git/guillotine/src/core/*.scala
	touch bootstrap/bin/guillotine

bootstrap/bin/exoskeleton: bootstrap/scala bootstrap/git/exoskeleton
	bootstrap/scala/bin/scalac -d bootstrap/bin -cp bootstrap/bin bootstrap/git/exoskeleton/src/core/*.scala
	touch bootstrap/bin/exoskeleton

bootstrap/bin/eucalyptus: bootstrap/scala bootstrap/git/eucalyptus
	bootstrap/scala/bin/scalac -d bootstrap/bin -cp bootstrap/bin bootstrap/git/eucalyptus/src/core/*.scala
	touch bootstrap/bin/eucalyptus

bootstrap/bin/kaleidoscope: bootstrap/scala bootstrap/git/kaleidoscope bootstrap/bin/contextual
	bootstrap/scala/bin/scalac -d bootstrap/bin -cp bootstrap/bin bootstrap/git/kaleidoscope/src/core/*.scala
	touch bootstrap/bin/kaleidoscope

bootstrap/bin/optometry: bootstrap/scala bootstrap/git/optometry
	bootstrap/scala/bin/scalac -d bootstrap/bin -cp bootstrap/bin bootstrap/git/optometry/src/core/*.scala
	touch bootstrap/bin/optometry

bootstrap/bin/escritoire: bootstrap/scala bootstrap/git/escritoire
	bootstrap/scala/bin/scalac -d bootstrap/bin -cp bootstrap/bin bootstrap/git/escritoire/src/core/*.scala
	touch bootstrap/bin/escritoire

bootstrap/bin/gastronomy: bootstrap/scala bootstrap/git/gastronomy bootstrap/bin/magnolia
	bootstrap/scala/bin/scalac -d bootstrap/bin -cp bootstrap/bin bootstrap/git/gastronomy/src/core/*.scala
	touch bootstrap/bin/gastronomy

bootstrap/bin/fury/strings: bootstrap/scala bootstrap/bin/contextual
	bootstrap/scala/bin/scalac -feature -d bootstrap/bin -cp bootstrap/bin:dist/bundle/lib/'*' src/strings/*.scala
	touch bootstrap/bin/fury/strings
	
bootstrap/bin/fury/io: bootstrap/scala bootstrap/bin/kaleidoscope bootstrap/bin/fury/strings
	bootstrap/scala/bin/scalac -feature -d bootstrap/bin -cp bootstrap/bin:dist/bundle/lib/'*' src/io/*.scala
	touch bootstrap/bin/fury/io

bootstrap/bin/fury/ogdl: bootstrap/scala bootstrap/bin/magnolia bootstrap/bin/fury/io bootstrap/bin/fury/strings
	bootstrap/scala/bin/scalac -feature -d bootstrap/bin -cp bootstrap/bin:dist/bundle/lib/'*' src/ogdl/*.scala
	touch bootstrap/bin/fury/ogdl

bootstrap/bin/fury/jsongen: bootstrap/scala bootstrap/bin/fury/strings
	bootstrap/scala/bin/scalac -feature -d bootstrap/bin -cp bootstrap/bin:dist/bundle/lib/'*' src/jsongen/*.scala
	touch bootstrap/bin/fury/jsongen

bootstrap/bin/fury/core: dist/bundle/bin/launcher bootstrap/scala $(NAILGUNJARPATH) dependency-jars $(REPOS) $(SRCS) bootstrap/bin/fury/strings bootstrap/bin/fury/io bootstrap/bin/fury/jsongen bootstrap/bin/fury/ogdl bootstrap/bin/gastronomy bootstrap/bin/optometry bootstrap/bin/exoskeleton bootstrap/bin/guillotine bootstrap/bin/escritoire bootstrap/bin/eucalyptus
	bootstrap/scala/bin/scalac -feature -d bootstrap/bin -cp bootstrap/bin:dist/bundle/lib/'*' src/core/*.scala
	touch bootstrap/bin/fury/core

bootstrap/bin/fury/module: bootstrap/scala bootstrap/bin/fury/core
	bootstrap/scala/bin/scalac -feature -d bootstrap/bin -cp bootstrap/bin:dist/bundle/lib/'*' src/module/*.scala
	touch bootstrap/bin/fury/module

bootstrap/bin/fury/project: bootstrap/scala bootstrap/bin/fury/core
	bootstrap/scala/bin/scalac -feature -d bootstrap/bin -cp bootstrap/bin:dist/bundle/lib/'*' src/project/*.scala
	touch bootstrap/bin/fury/project

bootstrap/bin/fury/dependency: bootstrap/scala bootstrap/bin/fury/core
	bootstrap/scala/bin/scalac -feature -d bootstrap/bin -cp bootstrap/bin:dist/bundle/lib/'*' src/dependency/*.scala
	touch bootstrap/bin/fury/dependency

bootstrap/bin/fury/schema: bootstrap/scala bootstrap/bin/fury/core
	bootstrap/scala/bin/scalac -feature -d bootstrap/bin -cp bootstrap/bin:dist/bundle/lib/'*' src/schema/*.scala
	touch bootstrap/bin/fury/schema

bootstrap/bin/fury/source: bootstrap/scala bootstrap/bin/fury/core
	bootstrap/scala/bin/scalac -feature -d bootstrap/bin -cp bootstrap/bin:dist/bundle/lib/'*' src/source/*.scala
	touch bootstrap/bin/fury/source

bootstrap/bin/fury/build: bootstrap/scala bootstrap/bin/fury/core
	bootstrap/scala/bin/scalac -feature -d bootstrap/bin -cp bootstrap/bin:dist/bundle/lib/'*' src/build/*.scala
	touch bootstrap/bin/fury/build

bootstrap/bin/fury/repo: bootstrap/scala bootstrap/bin/fury/core
	bootstrap/scala/bin/scalac -feature -d bootstrap/bin -cp bootstrap/bin:dist/bundle/lib/'*' src/repo/*.scala
	touch bootstrap/bin/fury/repo

bootstrap/bin/fury: bootstrap/scala bootstrap/bin/fury/repo bootstrap/bin/fury/build bootstrap/bin/fury/source bootstrap/bin/fury/schema bootstrap/bin/fury/dependency bootstrap/bin/fury/project bootstrap/bin/fury/module
	bootstrap/scala/bin/scalac -feature -d bootstrap/bin -cp bootstrap/bin:dist/bundle/lib/'*' src/module/*.scala
	touch bootstrap/bin/fury

# Libraries

dist/bundle/lib:
	mkdir -p $@

dist/bundle/lib/$(NAILGUNJAR):
	mkdir -p dist/bundle/lib
	curl -o $@ http://central.maven.org/maven2/com/facebook/nailgun-server/1.0.0/nailgun-server-1.0.0.jar

all-jars: $(JARS)

dist/bundle/lib/fury.jar: bootstrap/bin/fury
	echo "$(VERSION)" > bootstrap/bin/fury/.version
	jar -cf $@ -C bootstrap/bin fury
	jar -uf $@ -C bootstrap/bin/fury .version

dist/bundle/lib/%.jar: dist/bundle/lib bootstrap/bin/%
	jar -cf $@ -C bootstrap/bin $*

# Binaries

dist/bundle/bin/fury: $(foreach D, $(BINDEPS), dist/bundle/bin/$(D))
	cp etc/fury $@
	chmod +x $@

dist/bundle/bin/coursier:
	mkdir -p dist/bundle/bin
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

dist/bundle/bin/launcher: dist/bundle/bin/coursier
	mkdir -p dist/bundle/bin
	$< bootstrap --quiet -f --deterministic --output $@ ch.epfl.scala:bloop-launcher_2.12:$(BLOOPVERSION)

dist/bundle/bin/ng.c:
	mkdir -p bootstrap/ng
	curl -s -L -o $@ https://raw.githubusercontent.com/facebook/nailgun/master/nailgun-client/c/ng.c

dist/bundle/bin/ng.py:
	mkdir -p dist/bundle/bin
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

test: $(TESTS)

integration:
	etc/integration

test-isolated: ci
	@docker run -w /build -t $(DOCKER_TAG) make test

integration-isolated: ci
	@docker run -u bash_user -w /home/bash_user -t $(DOCKER_TAG) /build/etc/integration

integration-isolated-no-rebuild:
	@docker run -u bash_user -w /home/bash_user -t $(DOCKER_TAG) /build/etc/integration

ci:
	docker build -t $(DOCKER_TAG) .

clean-ci:
	docker build --no-cache -t fury-ci .

clean-dist:
	rm -rf dist

clean: clean-dist
	rm -rf bootstrap

download: $(REPOS) dist/bundle/bin/coursier dist/bundle/bin/ng.py dist/bundle/bin/ng.c dist/bundle/bin/launcher dist/bundle/lib/$(NAILGUNJAR) bootstrap/scala
	dist/bundle/bin/launcher --skip-bsp-connection $(BLOOPVERSION) # to download bloop

install: dist/install.sh
	dist/install.sh

.PHONY: all publish compile watch bloop-clean clean-compile clean-dist clean test ci clean-ci test-isolated integration-isolated integration $(TESTS) all-jars download install dependency-jars
