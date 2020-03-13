VERSION="${shell git describe --tags 2> /dev/null}"
LAYER_REF="QmegLbqVdLgis1wUiB2s5HRxpYZXS9343PkiJ7JPBkBCaJ"

run: dist/fury tmp/.bundle.ipfs

fury: dist/fury publish
	@printf "Copying new launcher script to root directory..." && \
	 cp "$<" "$@" && \
	 printf "done\n"

clean:
	@printf "Cleaning tmp, dist directories..." && \
	 rm -rf tmp dist && \
	 printf "done\n" || printf "failed\n"

uninstall:
	@printf "Removing all previous installations of Fury..."
	@rm -rf $(HOME)/.local/share/fury/usr $(HOME)/.local/share/fury/downloads && \
	 printf "done\n" || printf "failed\n"

install: etc/launcher uninstall dist/fury.tar.gz 
	@( printf "Rewriting Fury launcher script for local use..." && \
	   mkdir -p dist && \
	   sed "s/%VERSION%/$(VERSION)/" "$<" > "$@.tmp" && \
	   sed "s/%HASH%//" "$@.tmp" > "tmp/fury" && \
	   chmod +x "tmp/fury" && \
	   printf "done\n" || printf "failed\n" \
	 ) && \
	 ( printf "Installing Fury...\n" && \
	   mkdir -p ~/.local/share/fury/usr/$(VERSION) && \
	   tar xf dist/fury.tar.gz -C ~/.local/share/fury/usr/$(VERSION) && \
	   tmp/fury system install && \
	   printf "Done\n" || printf "Failed\n" \
	 )

tmp/.version:
	@printf "Writing current version ($(VERSION)) to a file..." && \
	 mkdir -p tmp && \
	 printf "$(VERSION)" > "$@" && \
	 printf "done\n" || printf "failed\n"

tmp/lib/fury.jar: $(wildcard **/*.scala) tmp/.version
	@printf "Cloning the Fury layer over IPFS...\n" && \
	 mkdir -p tmp/lib && \
	 ./fury layer clone -d . -l fury://$(LAYER_REF) && \
	 printf "Done\n" && \
	 printf "Compiling Fury from source...\n" && \
	 ./fury build run --https --project fury --module frontend --output linear --dir tmp/lib --fat-jar --disable-security-manager && \
	 mv tmp/lib/fury-frontend.jar "$@" && \
	 jar uf "$@" -C tmp .version && \
	 touch "$@" && \
	 printf "Done\n" || printf "Failed\n"

tmp/bin/fury: etc/fury
	@printf "Copying Fury runner script..." && \
	 mkdir -p tmp/bin && \
	 cp "$<" "$@" && \
	 printf "done\n" || printf "failed\n"

tmp/script/_fury: etc/completion/zsh/_fury
	@printf "Copying zsh completion script..." && \
	 mkdir -p tmp/script && \
	 cp "$<" "$@" && \
	 printf "done\n" || printf "failed\n"

tmp/bin/procname.c: etc/procname.c
	@printf "Copying Procname C wrapper..." && \
	 mkdir -p tmp/bin && \
	 cp "$<" "$@" && \
	 printf "done\n" || printf "failed\n"

tmp/bin/ng.c:
	@printf "Downloading Nailgun C client..." && \
	 mkdir -p tmp/bin && \
	 curl -Lso "$@" https://raw.githubusercontent.com/facebook/nailgun/master/nailgun-client/c/ng.c && \
	 printf "done\n" || printf "failed\n"

tmp/bin/ng.py:
	@printf "Downloading Nailgun Python client..." && \
	 mkdir -p tmp/bin && \
	 curl -Lso "$@" https://raw.githubusercontent.com/facebook/nailgun/master/nailgun-client/py/ng.py && \
	 sed -i.bak '1 s/$$/2.7/' "$@" && rm "$@.bak" && \
	 chmod +x "$@" && \
	 printf "done\n" || printf "failed\n"

dist/fury.tar.gz: tmp/.version tmp/lib/fury.jar tmp/bin/fury tmp/bin/ng.c tmp/bin/ng.py tmp/bin/procname.c tmp/script/_fury
	@printf "Creating bundle file..." && \
	 mkdir -p dist && \
	 tar czf "$@" -C tmp .version lib bin script && \
	 printf "done\n" || printf "failed\n"

tmp/.bundle.ipfs: dist/fury.tar.gz
	@printf "Adding $(shell tput -Tansi sitm)Fury bundle $(VERSION)$(shell tput -Tansi sgr0) to IPFS..." & \
	 ipfs add -q "$<" > "$@" && \
	 printf "done\n" || printf "failed\n"

dist/fury: etc/launcher tmp/.bundle.ipfs
	@printf "Rewriting Fury launcher script..." && \
	 mkdir -p dist && \
	 sed "s/%VERSION%/$(VERSION)/" "$<" > "$@.tmp" && \
	 sed "s/%HASH%/$(shell cat tmp/.bundle.ipfs)/" "$@.tmp" > "$@" && \
	 chmod +x "$@" && \
	 printf "done\n" || printf "failed\n"

tmp/.launcher.ipfs: dist/fury
	@printf "Adding $(shell tput -Tansi sitm)Fury launcher $(VERSION)$(shell tput -Tansi sgr0) to IPFS..." && \
	 ipfs add -q "$<" > "$@" && \
	 printf "done\n" || printf "failed\n"

pinata: tmp/.bundle.ipfs .pinata/apiKey .pinata/secretApiKey
	@( echo $(VERSION) | grep -q '-' && printf "Not pinning snapshot release of Fury bundle.\n" ) || \
	 ( printf "Sending $(shell tput -Tansi sitm)addHashToPinQueue$(shell tput -Tansi sgr0) request to Pinata..." && \
	   curl -s \
	    -X POST \
	    -H "Content-Type: application/json" \
	    -H "pinata_api_key: $(shell cat .pinata/apiKey)" \
	    -H "pinata_secret_api_key: $(shell cat .pinata/secretApiKey)" \
	    -d "{\"pinataMetadata\":{\"name\":\"fury-$(VERSION).tar.gz\"},\"hashToPin\":\"$(shell cat tmp/.bundle.ipfs)\"}" \
	    "https://api.pinata.cloud/pinning/addHashToPinQueue" > /dev/null && \
	    printf "done\n" || printf "failed\n" \
	 ) && \
	 printf "$(shell tput -Tansi bold)Fury bundle version $(VERSION) published to $(shell cat tmp/.bundle.ipfs)$(shell tput -Tansi sgr0)\n"

publish: tmp/.launcher.ipfs pinata .pinata/apiKey .pinata/secretApiKey
	@( echo $(VERSION) | grep -q '-' && printf "Not pinning snapshot release of Fury launcher.\n" ) || \
	 ( printf "Sending $(shell tput -Tansi sitm)addHashToPinQueue$(shell tput -Tansi sgr0) request to Pinata..." && \
	   curl -s \
	    -X POST \
	    -H "Content-Type: application/json" \
	    -H "pinata_api_key: $(shell cat .pinata/apiKey)" \
	    -H "pinata_secret_api_key: $(shell cat .pinata/secretApiKey)" \
	    -d "{\"pinataMetadata\":{\"name\":\"fury-$(VERSION).sh\"},\"hashToPin\":\"$(shell cat tmp/.launcher.ipfs)\"}" \
	    "https://api.pinata.cloud/pinning/addHashToPinQueue" > /dev/null && \
	    printf "done\n" && \
	    printf "Copying new launcher script to root directory..." && \
	    cp dist/fury fury && \
	    printf "done\n" || printf "failed\n" \
	 )
	@printf "$(shell tput -Tansi bold)Fury launcher $(VERSION) published to $(shell cat tmp/.launcher.ipfs)$(shell tput -Tansi sgr0)\n"

.PHONY: run publish pinata pinata-launcher clean uninstall
