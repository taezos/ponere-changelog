# ponere-changelog

## How to use
```
ponere-changelog: Manage CHANGELOG.md file

Usage: ponere-changelog COMMAND
  Use ponere-changelog to manage the CHANGELOG.md

Available options:
  -h,--help                Show this help text
  -v,--version             Show version

Available commands:
  update                   Update CHANGELOG file
  read                     Render CHANGELOG file
```

## How it works

When the commands are ran inside the project directory it will retreive all the commit messages from the last tag all the way to `HEAD`,
then together with the latest tag and date, renders it to `CHANGELOG.md` as hints. This can serve as clues on what to
write on changelog.

## Installation with Cabal
Clone the repo
```
# github repo
git clone https://github.com/taezos/ponere-changelog.git

# taezos repo
git clone https://taezos.org/taezos/ponere-changelog.git
```
Build and Install
```
cd ./ponere-changelog
cabal new-build
cabal new-install
```
This will get installed in `~/.cabal/bin`

## Installation with Nix
Clone the repo
```
# github repo
git clone https://github.com/taezos/ponere-changelog.git

# taezos repo
git clone https://taezos.org/taezos/ponere-changelog.git
```
Install with nix
```
cd ./ponere-changelog
nix-env -f release.nix -i
```
