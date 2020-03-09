VERSION="${shell git describe --tags 2> /dev/null}"
LAYER_REF="QmXrEsyVnUw1LXPkHmbQTyuM7x8eJyQYjcEY7DJcjQ39zY"

run: dist/fury tmp/.bundle.ipfs

publish: run pinata

fury:
	@printf "Copying new launcher script to root directory..."
	@cp "$<" "$@" && \
	 printf "done\n"

pinata-launcher: tmp/.launcher.ipfs
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

pinata: dist/fury.tar.gz tmp/.bundle.ipfs .pinata/apiKey .pinata/secretApiKey
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

uninstall:
	@printf "Removing all previous installations of Fury..."
	@rm -rf $(HOME)/.local/share/fury/usr $(HOME)/.local/share/fury/downloads && \
	 printf "done\n"

install: clean uninstall dist/fury dist/fury.tar.gz 
	@printf "Installing Fury...\n"
	@mkdir -p ~/.local/share/fury/usr/$(VERSION)
	@tar xf dist/fury.tar.gz -C ~/.local/share/fury/usr/$(VERSION)
	@dist/fury system install && \
	 printf "Done\n"
	@fury stop 2> /dev/null

dist/fury: etc/launcher tmp/.bundle.ipfs
	@printf "Rewriting Fury launcher script..."
	@mkdir -p dist && \
	 sed "s/%VERSION%/$(VERSION)/" "$<" > "$@.tmp" && \
	 sed "s/%HASH%/$(shell cat tmp/.bundle.ipfs)/" "$@.tmp" > "$@" && \
	 chmod +x "$@" && \
	 printf "done\n"

tmp/.version:
	@printf "Writing current version ($(VERSION)) to a file..."
	@mkdir -p tmp && \
	 printf "$(VERSION)" > "$@" && \
	 printf "done\n"

tmp/lib/fury.jar: fury $(wildcard **/*.scala) tmp/.version
	@printf "Cloning the Fury layer over IPFS...\n"
	@mkdir -p tmp/lib && \
	 ./fury layer clone -d . -l fury://$(LAYER_REF) && \
	 printf "Done\n" && \
	 printf "Compiling Fury from source...\n" && \
	 echo $(LANG) && \
	 ./fury build run --https --project fury --module frontend --output linear --dir tmp/lib --fat-jar --disable-security-manager && \
	 mv tmp/lib/fury-frontend.jar "$@" && \
	 jar uf "$@" -C tmp .version && \
	 touch "$@" && \
	 printf "Done\n"

tmp/bin/fury: etc/fury
	@printf "Copying Fury runner script..."
	@mkdir -p tmp/bin && \
	 cp "$<" "$@" && \
	 printf "done\n"

tmp/script/_fury: etc/completion/zsh/_fury
	@printf "Copying zsh completion script..."
	@mkdir -p tmp/script && \
	 cp "$<" "$@" && \
	 printf "done\n"

tmp/bin/procname.c: etc/procname.c
	@printf "Copying Procname C wrapper..."
	@mkdir -p tmp/bin && \
	 cp "$<" "$@" && \
	 printf "done\n"

tmp/bin/ng.c:
	@printf "Downloading Nailgun C client..."
	@mkdir -p tmp/bin && \
	 curl -Lso "$@" https://raw.githubusercontent.com/facebook/nailgun/master/nailgun-client/c/ng.c && \
	 printf "done\n"

tmp/bin/ng.py:
	@printf "Downloading Nailgun Python client..."
	@mkdir -p tmp/bin && \
	 curl -Lso "$@" https://raw.githubusercontent.com/facebook/nailgun/master/nailgun-client/py/ng.py && \
	 sed -i.bak '1 s/$$/2.7/' "$@" && rm "$@.bak" && \
	 chmod +x "$@" && \
	 printf "done\n"

tmp/.bundle.ipfs: dist/fury.tar.gz
	@printf "Adding $(shell tput sitm)Fury bundle $(VERSION)$(shell tput sgr0) to IPFS..."
	@ipfs add -q "$<" > "$@" && \
	 printf "done\n"

tmp/.launcher.ipfs: dist/fury
	@printf "Adding $(shell tput sitm)Fury launcher $(VERSION)$(shell tput sgr0) to IPFS..."
	@ipfs add -q "$<" > "$@" && \
	 printf "done\n"

dist/fury.tar.gz: tmp/.version tmp/lib/fury.jar tmp/bin/fury tmp/bin/ng.c tmp/bin/ng.py tmp/bin/procname.c tmp/script/_fury
	@printf "Creating bundle file..."
	@mkdir -p dist && \
	 tar czf "$@" -C tmp .version lib bin script && \
	 printf "done\n"

.PHONY: run publish pinata pinata-launcher clean uninstall
