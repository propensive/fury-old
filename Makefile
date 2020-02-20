VERSION=${shell git describe --tags 2> /dev/null}
FURYSTABLE="0.8.2"

dist/fury: etc/fury.sh dist publish
	sed "s/%VERSION%/$(VERSION)/" "$<" > "$@.tmp" && \
	sed "s/%HASH%/$(shell cat tmp/.url)/" "$@.tmp" > "$@" && \
	chmod +x "$@"

opt:
	mkdir -pinata "$@"

opt/fury.sh: opt
	curl -C - -o# "$@" "http://downloads.furore.dev/fury-$(FURYSTABLE).sh" && \
	chmod +x "$@"

opt/fury/bin/fury: opt/fury.sh
	"$<" "opt/fury" && touch "$@"

tmp:
	mkdir -p "$@"

tmp/lib: tmp
	mkdir -p "$@"

tmp/.version: tmp
	echo "$(VERSION)" > "$@"

tmp/lib/fury.jar: opt/fury/bin/fury tmp/lib $(wildcard **/*.scala) tmp/.version
	opt/fury/bin/fury build run --project fury --module frontend --dir tmp/lib --fat-jar && \
	mv tmp/lib/fury-frontend.jar "$@" && \
	jar uf "$@" -C tmp .version

tmp/bin: tmp
	mkdir -p "$@"

tmp/bin/fury: etc/fury tmp/bin
	cp "$<" "$@"

tmp/bin/ng.c: tmp/bin
	curl -Lso "$@" https://raw.githubusercontent.com/facebook/nailgun/master/nailgun-client/c/ng.c

tmp/bin/ng.py: tmp/bin
	curl -Lso "$@" https://raw.githubusercontent.com/facebook/nailgun/master/nailgun-client/py/ng.py && \
	sed -i.bak '1 s/$$/2.7/' "$@" && rm "$@.bak" && \
	chmod +x "$@"

tmp/.url: dist/fury-$(VERSION).tar.gz
	ipfs add -q "$<" > "$@"

dist:
	mkdir -p "$@"

dist/fury-$(VERSION).tar.gz: dist tmp/.version tmp/lib/fury.jar tmp/bin/fury tmp/bin/ng.c tmp/bin/ng.py
	tar czf "$@" -C tmp .version lib bin

publish: dist/fury-$(VERSION).tar.gz tmp/.url .pinata/apiKey .pinata/secretApiKey
	curl -s \
	-X POST \
	-H "Content-Type: application/json" \
	-H "pinata_api_key: $(shell cat .pinata/apiKey)" \
	-H "pinata_secret_api_key: $(shell cat .pinata/secretApiKey)" \
	-d "{\"pinataMetadata\":{\"name\":\"fury-$(VERSION).tar.gz\"},\"hashToPin\":\"$(shell cat tmp/.url)\"}" \
	"https://api.pinata.cloud/pinning/addHashToPinQueue"

clean:
	rm -rf tmp dist

.PHONY: publish clean
