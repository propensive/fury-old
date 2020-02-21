VERSION="${shell git describe --tags 2> /dev/null}"
LAYER_REF="QmfJSMCAtA1CR9uDubPQQsB8b32vrMJpJrrFY3u49n4tpb"

publish-launcher: tmp/.launcher.ipfs
	@echo -n "Sending $(shell tput sitm)addHashToPinQueue$(shell tput sgr0) request to Pinata..."
	@curl -s \
		-X POST \
		-H "Content-Type: application/json" \
		-H "pinata_api_key: $(shell cat .pinata/apiKey)" \
		-H "pinata_secret_api_key: $(shell cat .pinata/secretApiKey)" \
		-d "{\"pinataMetadata\":{\"name\":\"fury-$(VERSION).sh\"},\"hashToPin\":\"$(shell cat tmp/.launcher.ipfs)\"}" \
		"https://api.pinata.cloud/pinning/addHashToPinQueue" > /dev/null && \
		echo "done" && \
		echo "$(shell tput bold)Fury launcher $(VERSION) published to $(shell cat tmp/.launcher.ipfs)$(shell tput sgr0)"

dist/fury: etc/launcher dist publish
	@echo -n "Rewriting Fury launcher script..."
	@sed "s/%VERSION%/$(VERSION)/" "$<" > "$@.tmp" && \
		sed "s/%HASH%/$(shell cat tmp/.bundle.ipfs)/" "$@.tmp" > "$@" && \
		chmod +x "$@" && \
		echo "done"

tmp:
	@echo -n "Making $(shell tput sitm)$@$(shell tput sgr0) directory..."
	@mkdir -p "$@" && \
		echo "done"

tmp/lib: tmp
	@echo -n "Making $(shell tput sitm)$@$(shell tput sgr0) directory..."
	@mkdir -p "$@" && \
		echo "done"

tmp/.version: tmp
	@echo -n "Writing current version ($(VERSION)) to a file..."
	@echo "$(VERSION)" > "$@" && \
		echo "done"

tmp/lib/fury.jar: fury tmp/lib $(wildcard **/*.scala) tmp/.version
	@echo "Compiling Fury from source..."
	@./fury layer clone -d . -l fury://$(LAYER_REF)
	@./fury build run --project fury --module frontend --dir tmp/lib --fat-jar --disable-security-manager && \
		mv tmp/lib/fury-frontend.jar "$@" && \
		jar uf "$@" -C tmp .version && \
		touch "$@"

tmp/bin: tmp
	@echo -n "Making $(shell tput sitm)$@$(shell tput sgr0) directory..."
	@mkdir -p "$@" && \
		echo "done"

tmp/bin/fury: etc/fury tmp/bin
	@echo -n "Copying Fury runner script..."
	@cp "$<" "$@" && \
		echo "done"

tmp/bin/ng.c: tmp/bin
	@echo -n "Downloading Nailgun C client..."
	@curl -Lso "$@" https://raw.githubusercontent.com/facebook/nailgun/master/nailgun-client/c/ng.c && \
		echo "done"

tmp/bin/ng.py: tmp/bin
	@echo -n "Downloading Nailgun Python client..."
	@curl -Lso "$@" https://raw.githubusercontent.com/facebook/nailgun/master/nailgun-client/py/ng.py && \
		sed -i.bak '1 s/$$/2.7/' "$@" && rm "$@.bak" && \
		chmod +x "$@" && \
		echo "done"

tmp/.bundle.ipfs: dist/fury-$(VERSION).tar.gz
	@echo -n "Adding $(shell tput sitm)Fury bundle $(VERSION)$(shell tput sgr0) to IPFS..."
	@ipfs add -q "$<" > "$@" && \
		echo "done"

tmp/.launcher.ipfs: dist/fury
	@echo -n "Adding $(shell tput sitm)Fury launcher $(VERSION)$(shell tput sgr0) to IPFS..."
	@ipfs add -q "$<" > "$@" && \
		echo "done"

dist:
	@echo -n "Making $(shell tput sitm)$@$(shell tput sgr0) directory..."
	@mkdir -p "$@" && \
		echo "done"

dist/fury-$(VERSION).tar.gz: dist tmp/.version tmp/lib/fury.jar tmp/bin/fury tmp/bin/ng.c tmp/bin/ng.py
	@echo -n "Creating bundle file..."
	@tar czf "$@" -C tmp .version lib bin && \
		echo "done"

publish: dist/fury-$(VERSION).tar.gz tmp/.bundle.ipfs .pinata/apiKey .pinata/secretApiKey
	@echo -n "Sending $(shell tput sitm)addHashToPinQueue$(shell tput sgr0) request to Pinata..."
	@curl -s \
		-X POST \
		-H "Content-Type: application/json" \
		-H "pinata_api_key: $(shell cat .pinata/apiKey)" \
		-H "pinata_secret_api_key: $(shell cat .pinata/secretApiKey)" \
		-d "{\"pinataMetadata\":{\"name\":\"fury-$(VERSION).tar.gz\"},\"hashToPin\":\"$(shell cat tmp/.bundle.ipfs)\"}" \
		"https://api.pinata.cloud/pinning/addHashToPinQueue" > /dev/null && \
		echo "done" && \
		echo "$(shell tput bold)Fury bundle version $(VERSION) published to $(shell cat tmp/.bundle.ipfs)$(shell tput sgr0)"

clean:
	@echo -n "Cleaning tmp, dist directories..."
	@rm -rf tmp dist && \
		echo "done"

.PHONY: publish publish-launcher publish-layer clean