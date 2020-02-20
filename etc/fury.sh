#!/bin/bash

thisVersion="%VERSION%"
ipfsHash="%HASH%"
startTime="$(($(date "+%s%N")/1000000))"
xdgUsrHome="${XDG_DATA_HOME:-"$HOME/.local/share"}"
xdgSysHome="${XDG_DATA_HOME:-/usr/share}"
xdgHome="$([ "$EUID" = "0" ] && echo "${xdgSysHome}" || echo "${xdgUsrHome}")"
furyDir="${xdgHome}/fury"
furyUsr="${furyDir}/usr"
currentDir="${furyUsr}/current"
installedVersion="$(cat "${currentDir}/.version" 2> /dev/null || echo "")"
currentVersionedDir="${furyUsr}/${installedVersion}"
downloadDir="${furyDir}/downloads"
downloadFile="${downloadDir}/fury-${thisVersion}.tar.gz"
ipfsGateway="https://gateway.pinata.cloud/ipfs"
downloadUrl="${ipfsGateway}/${ipfsHash}"
installDir="${furyUsr}/$([ "${installedVersion}" = "${thisVersion}" ] && echo "current" || echo "${thisVersion}")"
system="$(uname -sm)"
args="$@"

stderrEcho() {
  printf "$1\n" >&2
}

fail() {
  message="$1"
  stderrEcho "${message}" && exit 1
}

downloadFuryTar() {
  mkdir -p "${downloadDir}"
  stderrEcho "Downloading Fury ${thisVersion}"
  [ -f "${downloadFile}" ] || curl -# -Lso "${downloadFile}" "${downloadUrl}" || fail "Could not download Fury version ${thisVersion}"
  stderrEcho "Download complete"
}

untarFury() {
  stderrEcho "Extracting Fury ${thisVersion} into ${installDir}"
  mkdir -p "${installDir}" && tar xf "${downloadFile}" -C "${installDir}" || fail "Could not extract Fury"
}

installFury() {
  [ "${currentVersion}" = "${thisVersion}" ] || (downloadFuryTar && untarFury)
}

runFury() {
  FURY_HOME="${installDir}" START_TIME="${startTime}" "${installDir}/bin/fury" "${args}"
}

([ -d "${installDir}" ] || installFury) && runFury "$@"