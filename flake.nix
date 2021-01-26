{
  description = "A tasty project management solution";

  # To update all inputs:
  # $ nix flake update --recreate-lock-file
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/master";
  };

  outputs = { self, nixpkgs, flake-utils }:
  let
    overlay = self: super: {
      haskellPackages = super.haskellPackages.override {
        overrides = hself: hsuper: {
          saunf =
            hself.callCabal2nix
            "saunf"
            (self.nix-gitignore.gitignoreSourcePure
            [ ./.gitignore "flake.nix" ]
            ./.
            )
            { };
        };
      };
      saunf =
        self.haskell.lib.justStaticExecutables
        self.haskellPackages.saunf;
    };
  in
  flake-utils.lib.eachDefaultSystem (system:
  let
    pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; config = { allowBroken = true; }; };
  in
  {
    defaultPackage = pkgs.saunf;

    devShell = pkgs.haskellPackages.shellFor {
      packages = p: [
        p.saunf
      ];
      buildInputs = with pkgs.haskellPackages; [
        ghc
        cabal-install
        hlint
        pkgs.nixpkgs-fmt
      ];
      withHoogle = false;
    };
  }
  );
}
