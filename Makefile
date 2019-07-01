VERSION=${shell sh -c 'cat .version 2> /dev/null || git --git-dir git/fury/.git describe --exact-match --tags 2> /dev/null || git --git-dir git/fury/.git rev-parse --short HEAD'}
MKFILE := $(abspath $(lastword $(MAKEFILE_LIST)))
ROOTDIR := $(dir $(MKFILE))
BLOOPVERSION=1.3.2
DEPS=kaleidoscope optometry eucalyptus exoskeleton escritoire mercator magnolia gastronomy contextual guillotine canner
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
export PATH := $(PWD)/bootstrap/scala/bin:$(PATH)

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
	cat etc/install.sh $< > dist/install.sh
	sed -i "s/FURY_VERSION=test/FURY_VERSION=$(VERSION)/" dist/install.sh
	chmod +x dist/install.sh

dist/fury-$(VERSION).tar.gz: all-jars dist/bundle/bin/fury dist/bundle/etc
	tar czf $@ -C dist/bundle .

#TODO refactor etc structure (separate bundled scripts from development ones)
dist/bundle/etc:
	mkdir -p $@
	cp -r etc/aliases etc/bashrc etc/fishrc etc/zshrc etc/completion etc/security $@

# Compilation

bootstrap/scala:
	mkdir -p $@
	curl -s https://downloads.lightbend.com/scala/2.12.8/scala-2.12.8.tgz | tar xz -C $@ --strip 1

bootstrap/git/canner:
	mkdir -p $@
	git clone --recursive https://github.com/odisseus/canner.git $@ --branch=make

bootstrap/git/%:
	mkdir -p $@
	git clone --recursive https://github.com/propensive/$*.git $@ --branch=fury

dist/bundle/lib/%.jar: bootstrap/scala bootstrap/git/% dist/bundle/lib
	mkdir -p bootstrap/lib
	(cd bootstrap/git/$* && make)
	cp bootstrap/git/$*/lib/$*.jar $@

bootstrap/bin:
	mkdir -p $@

jmh_jars=org.openjdk.jmh:jmh-core:1.21 org.openjdk.jmh:jmh-generator-bytecode:1.21 org.openjdk.jmh:jmh-generator-reflection:1.21 org.openjdk.jmh:jmh-generator-asm:1.21
bsp_jars=org.eclipse.lsp4j:org.eclipse.lsp4j.jsonrpc:0.6.0 ch.epfl.scala:bsp4j:2.0.0-M4 ch.epfl.scala:bloop-launcher_2.12:$(BLOOPVERSION)
coursier_jars=io.get-coursier:coursier_2.12:1.1.0-M14-4
external_jars=$(jmh_jars) $(bsp_jars) $(coursier_jars)

dependency-jars: dist/bundle/bin/coursier dist/bundle/lib
	for JAR in $(shell dist/bundle/bin/coursier fetch -r sonatype:releases -r bintray:scalameta/maven -r bintray:scalacenter/releases $(external_jars)); do \
		cp $$JAR dist/bundle/lib/ ; \
	done

define compile-module =
scalac -d bootstrap/bin -cp dist/bundle/lib/'*':bootstrap/bin $@/*.scala
endef

pre-compile: dist/bundle/bin/launcher bootstrap/scala $(NAILGUNJARPATH) dependency-jars $(REPOS) $(foreach D,$(DEPS),dist/bundle/lib/$D.jar)

src/strings: pre-compile
	$(compile-module)

src/jsongen: src/strings
	$(compile-module)

src/io: src/strings
	$(compile-module)

src/ogdl: src/io
	$(compile-module)

src/core: src/jsongen src/ogdl
	$(compile-module)

src/module: src/core
	$(compile-module)

src/source: src/core
	$(compile-module)

src/schema: src/core
	$(compile-module)

src/project: src/core
	$(compile-module)

src/repo: src/core
	$(compile-module)

src/build: src/core
	$(compile-module)

src/dependency: src/core
	$(compile-module)

src/imports: src/core
	$(compile-module)

src/menu: src/schema src/repo src/dependency src/source src/build src/imports src/module src/project
	$(compile-module)

compile: src/menu

dist/bundle/bin/launcher: dist/bundle/bin/coursier dependency-jars dist/bundle/bin/.dir
	$< bootstrap --quiet -r bintray:scalacenter/releases -f --deterministic --output $@ ch.epfl.scala:bloop-launcher_2.12:$(BLOOPVERSION)

bootstrap/bin/fury/.version: bootstrap/bin/fury/.dir compile
	echo "$(VERSION)" > $@

# Libraries

dist/bundle/lib:
	mkdir -p $@

dist/bundle/lib/$(NAILGUNJAR): dist/bundle/lib
	curl -s -o $@ http://central.maven.org/maven2/com/facebook/nailgun-server/1.0.0/nailgun-server-1.0.0.jar

all-jars: $(JARS)

dist/bundle/lib/fury.jar: bootstrap/bin compile bootstrap/bin/fury/.version
	jar -cf $@ -C $< fury
	jar -uf $@ -C bootstrap/bin/fury .version

dist/bundle/lib/%.jar: bootstrap/bin bootstrap/bin/fury/.version dist/bundle/lib bootstrap/git/% compile
	jar -cf $@ -C $< $*

# Binaries

%/.dir:
	mkdir -p ${@D}
	touch ${@D}/.dir

dist/bundle/bin/fury: $(foreach D, $(BINDEPS), dist/bundle/bin/$(D)) $(foreach D, $(DEPS), dist/bundle/lib/$(D).jar)
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

fury-native: all-jars
	native-image -cp $(shell bash -c "ls $(NATIVEJARS) | paste -s -d: -") fury.Main

## Verification

$(TESTS): %-test: bootstrap/git/probably dist/bundle/lib/probably.jar dist/bundle/lib
	scalac -d bootstrap/bin -cp dist/bundle/lib/'*':bootstrap/bin src/$*-test/*.scala
	scala -cp dist/bundle/lib/'*':bootstrap/bin fury.Tests

test: $(TESTS)

integration:
	etc/integration

test-isolated: ci
	@docker run -w /build -t $(DOCKER_TAG) make test

integration-isolated: ci
	@docker run -u bash_user -w /home/bash_user -t $(DOCKER_TAG) /bin/bash -c 'source ~/.bashrc; /integration'

ci:
	docker build -t $(DOCKER_TAG) .

clean-ci:
	docker build --no-cache -t fury-ci .

clean-dist:
	rm -rf dist

clean: clean-dist
	rm -rf bootstrap

download: $(REPOS) dist/bundle/bin/coursier dist/bundle/bin/ng.py dist/bundle/bin/ng.c dist/bundle/bin/launcher dist/bundle/lib dist/bundle/lib/$(NAILGUNJAR) bootstrap/scala

install: dist/install.sh
	dist/install.sh

.PHONY: all publish compile pre-compile src/* watch bloop-clean clean-compile clean-dist clean test ci clean-ci test-isolated integration-isolated integration $(TESTS) all-jars download install dependency-jars
