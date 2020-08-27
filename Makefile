VERSION="${shell cat .version 2> /dev/null || git describe --tags 2> /dev/null}"
MK="[make]"
run: dist/fury tmp/.bundle.ipfs

fury: dist/fury publish
	@printf "$(MK) Copying new launcher script to root directory..." && \
	 cp "$<" "$@" && \
	 printf "done\n"

clean:
	@printf "$(MK) Cleaning tmp, dist directories..." && \
	 rm -rf .version tmp dist && \
	 printf "done\n" || (printf "failed\n" && exit 1)

uninstall:
	@printf "$(MK) Removing all previous installations of Fury..."
	@rm -rf $(HOME)/.local/share/fury/usr/$() $(HOME)/.local/share/fury/downloads && \
	 printf "done\n" || (printf "failed\n" && exit 1)

install: etc/launcher dist/fury.tar.gz 
	@( printf "$(MK) Rewriting Fury launcher script for local use..." && \
	   mkdir -p dist && \
	   ( sed "s/%VERSION%/$(VERSION)/" "$<" | sed "s/%HASH%//" > "tmp/fury" ) && \
	   chmod +x "tmp/fury" && \
	   printf "done\n" || (printf "failed\n" && exit 1) \
	 ) && \
	 ( printf "$(MK) Installing Fury...\n" && \
	   mkdir -p ~/.local/share/fury/usr/$(VERSION) && \
	   tar xf dist/fury.tar.gz -C ~/.local/share/fury/usr/$(VERSION) && \
	   tmp/fury system install && \
	   printf "$(MK) Done\n" || (printf "$(MK) Failed\n" && exit 1) \
	 )

tmp/.version:
	@printf "$(MK) Writing current version ($(VERSION)) to a file..." && \
	 mkdir -p tmp && \
	 printf "$(VERSION)\n" > "$@" && \
	 printf "$(shell date +%s)" >> "$@" && \
	 printf "done\n" || (printf "failed\n" && exit 1)

tmp/lib/fury.jar: $(wildcard src/**/*.scala) tmp/.version
	@printf "$(MK) Cloning the Fury layer over IPFS...\n" && \
	 mkdir -p tmp/lib && \
	 printf "$(MK) Done\n" && \
	 printf "$(MK) Compiling Fury from source...\n" && \
	 ./fury && \
	 ~/.local/share/fury/bin/fury build save --https --project fury --module frontend --output linear --path tmp/lib --fat-jar --disable-security-manager && \
	 mv tmp/lib/fury-frontend.jar "$@" && \
	 jar uf "$@" -C tmp .version && \
	 touch "$@" && \
	 printf "$(MK) Done\n" || (printf "$(MK) Failed\n" && exit 1)

tmp/bin/fury: etc/fury
	@printf "$(MK) Copying Fury runner script..." && \
	 mkdir -p tmp/bin && \
	 cp "$<" "$@" && \
	 printf "done\n" || printf "failed\n"

tmp/script/_fury: etc/completion/zsh/_fury
	@printf "$(MK) Copying zsh completion script..." && \
	 mkdir -p tmp/script && \
	 cp "$<" "$@" && \
	 printf "done\n" || (printf "failed\n" && exit 1)

tmp/bin/procname.c: etc/procname.c
	@printf "$(MK) Copying Procname C wrapper..." && \
	 mkdir -p tmp/bin && \
	 cp "$<" "$@" && \
	 printf "done\n" || (printf "failed\n" && exit 1)

tmp/bin/ng.c:
	@printf "$(MK) Downloading Nailgun C client..." && \
	 mkdir -p tmp/bin && \
	 curl -Lso "$@" https://raw.githubusercontent.com/facebook/nailgun/master/nailgun-client/c/ng.c && \
	 printf "done\n" || (printf "failed\n" && exit 1)

tmp/bin/ng.py:
	@printf "$(MK) Downloading Nailgun Python client..." && \
	 mkdir -p tmp/bin && \
	 curl -Lso "$@" https://raw.githubusercontent.com/facebook/nailgun/master/nailgun-client/py/ng.py && \
	 sed -i.bak '1 s/$$/2.7/' "$@" && rm "$@.bak" && \
	 chmod +x "$@" && \
	 printf "done\n" || (printf "failed\n" && exit 1)

tmp/etc: etc/icons/hicolor/16x16/apps/fury-icon.png etc/icons/hicolor/48x48/apps/fury-icon.png etc/icons/hicolor/128x128/apps/fury-icon.png 
	@printf "$(MK) Copying Fury icons..." && \
	 mkdir -p tmp/etc && \
	 cp -r etc/icons tmp/etc/icons && \
	 touch tmp/etc && \
	 printf "done\n" || (printf "failed\n" && exit 1)

icons: doc/logo/render_1000px.png
	@printf "$(MK) Resizing logo for Fury icons..." && \
	 mkdir -p etc/icons/hicolor/128x128/apps etc/icons/hicolor/48x48/apps etc/icons/hicolor/16x16/apps && \
	 convert doc/logo/render_1000px.png -resize 16x16 etc/icons/hicolor/16x16/apps/fury-icon.png && \
	 convert doc/logo/render_1000px.png -resize 48x48 etc/icons/hicolor/48x48/apps/fury-icon.png && \
	 convert doc/logo/render_1000px.png -resize 128x128 etc/icons/hicolor/128x128/apps/fury-icon.png && \
	 printf "done\n" || (printf "failed\n" && exit 1)

dist/fury.tar.gz: tmp/.version tmp/lib/fury.jar tmp/bin/fury tmp/bin/ng.c tmp/bin/ng.py tmp/bin/procname.c tmp/script/_fury tmp/etc tmp/etc
	@printf "$(MK) Creating bundle file..." && \
	 mkdir -p dist && \
	 tar czf "$@" -C tmp .version lib bin etc script && \
	 printf "done\n" || (printf "failed\n" && exit 1)

tmp/.bundle.ipfs: dist/fury.tar.gz
	@printf "$(MK) Adding $(shell tput -Tansi sitm)Fury bundle $(VERSION)$(shell tput -Tansi sgr0) to IPFS..." & \
	 ipfs add -q "$<" > "$@" && \
	 printf "done\n" || (printf "failed\n" && exit 1)

dist/fury: etc/launcher tmp/.bundle.ipfs
	@printf "$(MK) Rewriting Fury launcher script..." && \
	 mkdir -p dist && \
	 sed -e "s/%VERSION%/$(VERSION)/" \
	     -e "s/%HASH%/$(shell cat tmp/.bundle.ipfs)/" \
	     -e "s/%MD5%/$(shell md5sum dist/fury.tar.gz | head -c 32)/" "$<" > "$@" && \
	 chmod +x "$@" && \
	 printf "done\n" || (printf "failed\n" && exit 1)

tmp/.launcher.ipfs: dist/fury 
	@printf "$(MK) Adding $(shell tput -Tansi sitm)Fury launcher $(VERSION)$(shell tput -Tansi sgr0) to IPFS..." && \
	 ipfs add -q "$<" > "$@" && \
	 printf "done\n" || (printf "failed\n" && exit 1)

pinata: tmp/.bundle.ipfs .pinata/apiKey .pinata/secretApiKey
	@( echo $(VERSION) | grep -q '-' && printf "$(MK) Not pinning snapshot release of Fury bundle.\n" ) || \
	 ( printf "$(MK) Sending $(shell tput -Tansi sitm)addHashToPinQueue$(shell tput -Tansi sgr0) request to Pinata..." && \
	   curl -s \
	    -X POST \
	    -H "Content-Type: application/json" \
	    -H "pinata_api_key: $(shell cat .pinata/apiKey)" \
	    -H "pinata_secret_api_key: $(shell cat .pinata/secretApiKey)" \
	    -d "{\"pinataMetadata\":{\"name\":\"fury-$(VERSION).tar.gz\"},\"hashToPin\":\"$(shell cat tmp/.bundle.ipfs)\"}" \
	    "https://api.pinata.cloud/pinning/addHashToPinQueue" > /dev/null && \
	    printf "done\n" || ( printf "failed\n" && exit 1 ) \
	 ) && \
	 printf "$(shell tput -Tansi bold)Fury bundle version $(VERSION) published to $(shell cat tmp/.bundle.ipfs)$(shell tput -Tansi sgr0)\n"

.version:
	@( echo $(shell bash -c 'read -p "Please specify the new version (current: $(VERSION)): " V; echo $$V') > "$@" )

publish: .version pinata .pinata/apiKey .pinata/secretApiKey tmp/.launcher.ipfs
	@( echo $(VERSION) | grep -q '-' && printf "Not pinning snapshot release of Fury launcher.\n" ) || \
	 ( ( stat .version 2> /dev/null > /dev/null || \
	     ( printf "$(MK) Please specify the new version in the file $(shell tput -Tansi bold).version$(shell tput -Tansi sgr0).\n" && \
	       exit 1 ) \
	   ) && \
	   printf "$(MK) Checking there are no uncommitted changes..." && \
	   git diff-index HEAD -- && \
	   printf "done\n" && \
	   printf "$(MK) Updating source headers..." && \
	   etc/revise && \
	   printf "done\n" && \
	   git add src && \
	   git commit -m "Updated source headers to version $(VERSION)" && \
	   git push && \
	   git tag "$(VERSION)" && \
	   printf "$(MK) Sending $(shell tput -Tansi sitm)addHashToPinQueue$(shell tput -Tansi sgr0) request to Pinata..." && \
	   curl -s \
	    -X POST \
	    -H "Content-Type: application/json" \
	    -H "pinata_api_key: $(shell cat .pinata/apiKey)" \
	    -H "pinata_secret_api_key: $(shell cat .pinata/secretApiKey)" \
	    -d "{\"pinataMetadata\":{\"name\":\"fury-$(VERSION).sh\"},\"hashToPin\":\"$(shell cat tmp/.launcher.ipfs)\"}" \
	    "https://api.pinata.cloud/pinning/addHashToPinQueue" > /dev/null && \
	    printf "done\n" && \
	   git push --tags && \
	   printf "$(MK) Copying new launcher script to root directory..." && \
	   cp dist/fury fury && \
	   rm .version && \
	   git add fury && \
	   git commit -m "Updated bootstrap version to $(VERSION)" && \
	   git push && \
	   printf "$(MK) Done\n" || ( printf "$(MK) Failed\n" && exit 1 ) \
	 ) && \
	 printf "$(MK) $(shell tput -Tansi bold)Fury launcher $(VERSION) published to $(shell cat tmp/.launcher.ipfs)$(shell tput -Tansi sgr0)\n"

test:
	tmp/fury test --disable-security-manager --output linear

.PHONY: run publish pinata clean uninstall icons tmp/.version test
