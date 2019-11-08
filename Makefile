VERSION=${shell sh -c 'cat .version 2> /dev/null || git --git-dir git/fury/.git describe --exact-match --tags 2> /dev/null || git --git-dir git/fury/.git rev-parse --short HEAD'}
BLOOPVERSION=1.3.5
FURYSTABLE=0.7.3
FURYLOCAL=opt/fury-$(FURYSTABLE)/bin/fury
BINDEPS=coursier ng.py ng
NAILGUNJAR=nailgun-server-1.0.0.jar
NAILGUNJARPATH=dist/bundle/lib/$(NAILGUNJAR)
NATIVEJARS=dist/bundle/lib/fury-frontend.jar $(NAILGUNJARPATH) bootstrap/scala/lib/scala-library.jar bootstrap/scala/lib/scala-reflect.jar
DOCKER_TAG=fury-ci
export PATH := $(PWD)/bootstrap/scala/bin:$(PATH)

all: dist/bundle/lib/fury-frontend.jar

publish: dist/install.sh
	git tag "v$(VERSION)" -m "Version $(VERSION)"
	gsutil -h "Cache-Control:public,max-age=60" cp $< gs://revivalist/downloads/fury.build/fury-$(VERSION).sh
	git push --tags
	@echo
	@echo "To install this version of Fury, run:"
	@echo
	@echo "  curl -OL https://storage.googleapis.com/revivalist/downloads/fury.build/fury-$(VERSION).sh"
	@echo "  sh install-$(VERSION).sh"
	@echo

opt:
	mkdir -p opt

opt/fury-$(FURYSTABLE).tar.gz: opt
	if [ ! -f "$@" ]; then curl -s -o $@ "https://storage.googleapis.com/revivalist/downloads/fury.build/fury-$(FURYSTABLE).sh"; fi

$(FURYLOCAL): opt/fury-$(FURYSTABLE).tar.gz
	sh opt/fury-$(FURYSTABLE).tar.gz opt/fury-$(FURYSTABLE)

dist/install.sh: dist/fury-$(VERSION).tar.gz dist/bundle/etc
	cat etc/install.sh $< > dist/install.sh
	LC_ALL=C sed -i.bak "s/FURY_VERSION=test/FURY_VERSION=$(VERSION)/" dist/install.sh && rm dist/install.sh.bak
	chmod +x dist/install.sh

dist/fury-$(VERSION).tar.gz: dist/bundle/lib/fury-frontend.jar dist/bundle/bin/fury dist/bundle/etc
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

pre-compile: bootstrap/bin bootstrap/scala $(NAILGUNJARPATH)

# Libraries

dist/bundle/lib:
	mkdir -p $@

dist/bundle/lib/$(NAILGUNJAR): dist/bundle/lib
	curl -s -o $@ http://central.maven.org/maven2/com/facebook/nailgun-server/1.0.0/nailgun-server-1.0.0.jar

dist/bundle/lib/fury-frontend.jar: dist/bundle/lib $(FURYLOCAL) bootstrap/bin .version src/**/*.scala
	rm -f ~/.config/fury/layers/D2C9CF6A232DEFF07116A15501BB8DB0100F1C3D7BEB6B7ADEB780084B51F149
	rm -f ~/.config/fury/layers/29B89F711E5D63E22B8D798F55C6FF01A4E1638C12C4F1DD631DEE4CBC48EF2B
	rm -f ~/.config/fury/layers/0A24A1586682A39838622B66BACF81A3B51A7F3448371AB31B54B75DA12DCD28
	$(FURYLOCAL) standalone layer extract -f build.fury
	$(FURYLOCAL) standalone permission grant --module frontend --project fury -P 729
	$(FURYLOCAL) standalone build save --https --output linear --project fury --module frontend --dir $<
	jar -uf $@ .version

dist/bundle/lib/%.jar: bootstrap/bin .version dist/bundle/lib bootstrap/git/% compile
	jar -cf $@ -C $< $*

# Binaries

%/.dir:
	mkdir -p ${@D}
	touch ${@D}/.dir

dist/bundle/bin/fury: dist/bundle/bin/.dir $(foreach D, $(BINDEPS), dist/bundle/bin/$(D))
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

fury-native: dist/bundle/lib/fury-frontend.jar
	native-image -cp $(shell bash -c "ls $(NATIVEJARS) | paste -s -d: -") fury.Main

test:
	fury build compile --https --output linear --project fury --module test-all

integration:
	etc/integration

test-isolated: ci
	@docker run -w /build -t $(DOCKER_TAG) make test

integration-isolated: ci
	@docker run -u bash_user -w /home/bash_user -t $(DOCKER_TAG) /bin/bash -c 'source ~/.bashrc; /integration'

docker-console: ci
	@docker run -u bash_user -w /home/bash_user -ti $(DOCKER_TAG) /bin/bash

ci:
	docker build -t $(DOCKER_TAG) .

clean-ci:
	docker build --no-cache -t fury-ci .

clean: clean-dist
	rm -rf bootstrap dist opt

download: dist/bundle/bin/coursier dist/bundle/bin/ng.py dist/bundle/bin/ng.c dist/bundle/lib dist/bundle/lib/$(NAILGUNJAR) bootstrap/scala

install: dist/install.sh
	dist/install.sh

revise:
	etc/revise

.PHONY: all publish pre-compile clean-dist clean test ci clean-ci test-isolated integration-isolated integration download install revise
