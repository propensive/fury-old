#!/bin/sh

thisVersion="%VERSION%"
xdgHome="${XDG_DATA_HOME:-~/.local/share}"
furyDir="${xdgHome}/fury"
furyUsr="${furyDir}/usr"
currentDir="${furyUsr}/current"
installedVersion="$(cat "${currentDir}/.version")"
currentVersionedDir="${furyUsr}/${installedVersion}"
downloadDir="${furyDir}/downloads"
downloadFile="${downloadDir}/fury/${thisVersion}.tar.gz"
downloadUrl="https://propensive.com/downloads/fury/${thisVersion}.tar.gz"
installDir="${furyUsr}/$([ "${installedVersion}" = "${thisVersion}" ] && echo "current" || echo "${thisVersion}")"

stderrEcho() {
  echo "$1" >&2
}

fail() {
  stderrEcho "$1" && \
  exit 1
}

moveFury() {
  stderrEcho "Moving current installation of Fury to ${currentVersionedDir}"
  [ -e "${currentVersionedDir}" ] && rm -rf "${currentVersionedDir}"
  mv "${currentDir}" "${currentVersionedDir}"
}

downloadFuryTar() {
  stderrEcho "Downloading Fury version ${thisVersion}"
  [ -f "${downloadFile}" ] || \
      curl -Lso "${downloadFile}" "${downloadUrl}"
}

untarFury() {
  mkdir -p "${currentDir}" && \
  tar -C "${currentDir}" xf "${downloadFile}"
}

checkFuryIsInstalled() {
  [ -d "${currentDir}" ]
}

checkCurrentVersion() {
  [ "${currentVersion}" = "${thisVersion}" ]
}

installFury() {
  moveFury
  downloadFuryTar && untarFury
}



runFury() {
  FURY_HOME="${currentDir}" "${currentDir}/bin/fury" "$@"
}

(checkFuryIsInstalled || installFury) && runFury "$@"