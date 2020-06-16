{ mkDerivation, ansi-terminal, base, bytestring, file-embed, git
, microlens, mtl, optparse-applicative, relude, stdenv
, system-filepath, text, time, transformers, turtle
}:
mkDerivation {
  pname = "ponere-changelog";
  version = "0.1.1.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base bytestring file-embed git microlens mtl
    optparse-applicative relude system-filepath text time transformers turtle
  ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
