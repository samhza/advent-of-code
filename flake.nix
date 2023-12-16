{
  inputs = {
    nixpkgs = {
      url = "nixpkgs";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
  };
  outputs = { nixpkgs, flake-utils, ... }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
      };
    in rec {
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [
          haskell-language-server
          (haskell.packages.ghc948.ghcWithPackages(ps: with ps; [
            # parsec
            # vector
            # hoogle
          ]))
        ];
      };
    }
  );
}
