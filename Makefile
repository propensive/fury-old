VERSION="${shell git describe --tags 2> /dev/null}"
LAYER_REF="QmXrEsyVnUw1LXPkHmbQTyuM7x8eJyQYjcEY7DJcjQ39zY"

publish-launcher: tmp/.launcher.ipfs
	@printf "Sending $(shell tput sitm)addHashToPinQueue$(shell tput sgr0) request to Pinata..."
	@curl -s \
		-X POST \
		-H "Content-Type: application/json" \
		-H "pinata_api_key: $(shell cat .pinata/apiKey)" \
		-H "pinata_secret_api_key: $(shell cat .pinata/secretApiKey)" \
		-d "{\"pinataMetadata\":{\"name\":\"fury-$(VERSION).sh\"},\"hashToPin\":\"$(shell cat tmp/.launcher.ipfs)\"}" \
		"https://api.pinata.cloud/pinning/addHashToPinQueue" > /dev/null && \
		printf "done\n" && \
		printf "$(shell tput bold)Fury launcher $(VERSION) published to $(shell cat tmp/.launcher.ipfs)$(shell tput sgr0)\n"

dist/fury: etc/launcher dist publish
	@printf "Rewriting Fury launcher script..."
	@sed "s/%VERSION%/$(VERSION)/" "$<" > "$@.tmp" && \
		sed "s/%HASH%/$(shell cat tmp/.bundle.ipfs)/" "$@.tmp" > "$@" && \
		chmod +x "$@" && \
		printf "done\n"

tmp:
	@printf "Making $(shell tput sitm)$@$(shell tput sgr0) directory..."
	@mkdir -p "$@" && \
		printf "done\n"

tmp/lib: tmp
	@printf "Making $(shell tput sitm)$@$(shell tput sgr0) directory..."
	@mkdir -p "$@" && \
		printf "done\n"

tmp/.version: tmp
	@printf "Writing current version ($(VERSION)) to a file..."
	@printf "$(VERSION)" > "$@" && \
		printf "done\n"

tmp/lib/fury.jar: fury tmp/lib $(wildcard **/*.scala) tmp/.version
	@printf "Cloning the Fury layer over IPFS...\n"
	@./fury layer clone -d . -l fury://$(LAYER_REF) && \
		printf "Done\n" && \
		printf "Compiling Fury from source...\n" && \
		./fury build run --https --project fury --module frontend --dir tmp/lib --fat-jar --disable-security-manager && \
		mv tmp/lib/fury-frontend.jar "$@" && \
		jar uf "$@" -C tmp .version && \
		touch "$@" && \
		printf "Done\n"

tmp/bin: tmp
	@printf "Making $(shell tput sitm)$@$(shell tput sgr0) directory..."
	@mkdir -p "$@" && \
		printf "done\n"

tmp/bin/fury: etc/fury tmp/bin
	@printf "Copying Fury runner script..."
	@cp "$<" "$@" && \
		printf "done\n"

tmp/bin/ng.c: tmp/bin
	@printf "Downloading Nailgun C client..."
	@curl -Lso "$@" https://raw.githubusercontent.com/facebook/nailgun/master/nailgun-client/c/ng.c && \
		printf "done\n"

tmp/bin/ng.py: tmp/bin
	@printf "Downloading Nailgun Python client..."
	@curl -Lso "$@" https://raw.githubusercontent.com/facebook/nailgun/master/nailgun-client/py/ng.py && \
		sed -i.bak '1 s/$$/2.7/' "$@" && rm "$@.bak" && \
		chmod +x "$@" && \
		printf "done\n"

tmp/.bundle.ipfs: dist/fury-$(VERSION).tar.gz
	@printf "Adding $(shell tput sitm)Fury bundle $(VERSION)$(shell tput sgr0) to IPFS..."
	@ipfs add -q "$<" > "$@" && \
		printf "done\n"

tmp/.launcher.ipfs: dist/fury
	@printf "Adding $(shell tput sitm)Fury launcher $(VERSION)$(shell tput sgr0) to IPFS..."
	@ipfs add -q "$<" > "$@" && \
		printf "done\n"

dist:
	@printf "Making $(shell tput sitm)$@$(shell tput sgr0) directory..."
	@mkdir -p "$@" && \
		printf "done\n"

dist/fury-$(VERSION).tar.gz: dist tmp/.version tmp/lib/fury.jar tmp/bin/fury tmp/bin/ng.c tmp/bin/ng.py
	@printf "Creating bundle file..."
	@tar czf "$@" -C tmp .version lib bin && \
		printf "done\n"

publish: dist/fury-$(VERSION).tar.gz tmp/.bundle.ipfs .pinata/apiKey .pinata/secretApiKey
	@printf "Sending $(shell tput sitm)addHashToPinQueue$(shell tput sgr0) request to Pinata..."
	@curl -s \
		-X POST \
		-H "Content-Type: application/json" \
		-H "pinata_api_key: $(shell cat .pinata/apiKey)" \
		-H "pinata_secret_api_key: $(shell cat .pinata/secretApiKey)" \
		-d "{\"pinataMetadata\":{\"name\":\"fury-$(VERSION).tar.gz\"},\"hashToPin\":\"$(shell cat tmp/.bundle.ipfs)\"}" \
		"https://api.pinata.cloud/pinning/addHashToPinQueue" > /dev/null && \
		printf "done\n" && \
		printf "$(shell tput bold)Fury bundle version $(VERSION) published to $(shell cat tmp/.bundle.ipfs)$(shell tput sgr0)\n"

clean:
	@printf "Cleaning tmp, dist directories..."
	@rm -rf tmp dist && \
		printf "done\n"

.PHONY: publish publish-launcher publish-layer clean
