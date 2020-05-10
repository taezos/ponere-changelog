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

It will retreive all the commit messages from the last tag all the way to `HEAD`,
then renders it to `CHANGELOG.md` as hints. This can serve as clues on what to
write on changelog.
