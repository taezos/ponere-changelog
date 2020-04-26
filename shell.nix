{ nixpkgs ? import ./nix/pinned.nix {}, compiler ? "default", doBenchmark ? false  }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, git, microlens, mtl, stdenv
      , text, transformers, system-filepath, relude, ansi-terminal
      }:
      mkDerivation {
        pname = "umu-changelog";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base bytestring git microlens mtl text transformers system-filepath
          relude ansi-terminal
        ];
        executableHaskellDepends = [ base ];
        doHaddock = false;
        license = "unknown";
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
