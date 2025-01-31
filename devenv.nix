{ pkgs, ... }:
let compiler = "ghc965";
in {
  packages = with pkgs.haskell.packages."${compiler}"; [
    fourmolu
    cabal-fmt
    implicit-hie
  ];

  languages.haskell.enable = true;

  # Define scripts for common tasks
  scripts.buildSite = {
    description = "build hakyll site";
    exec = ''
      cabal run web -- rebuild
      if [ $? -eq 0 ]; then
        echo "Hakyll site rebuilt successfully."
      else
        echo "Failed to rebuild Hakyll site."
        exit 1
      fi
    '';
  };
}
