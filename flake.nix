{
  description = "Plain text software project management";

  # To update all inputs:
  # $ nix flake update --recreate-lock-file
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils/master";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      overlay = self: super: {
        haskellPackages = super.haskellPackages.override {
          overrides = hself: hsuper: {
            org-mode =
              hself.callCabal2nix
                "org-mode"
                (self.nix-gitignore.gitignoreSourcePure
                  [ ./.gitignore "flake.nix" ]
                  ./org-mode
                )
                { };

            saunf =
              hself.callCabal2nix
                "saunf"
                (self.nix-gitignore.gitignoreSourcePure
                  [ ./.gitignore "flake.nix" ]
                  ./saunf
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
            pkgs.ormolu
            pkgs.dhall
          ];
          withHoogle = false;
          GITHUB_TOKEN = "DEFAULT_FROM_FLAKE";
        };
      }
    );
}
