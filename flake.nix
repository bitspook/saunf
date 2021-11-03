{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    rust-overlay.url = "github:oxalica/rust-overlay";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs { inherit system overlays; };

        buildEnvVars = {
          OPENSSL_STATIC = 1;
        };

        buildInputs = with pkgs; [ openssl.dev ];

        nativeBuildInputs = with pkgs; [
          nixfmt
          postgresql_13
          pkgconfig
          rust-analyzer
          diesel-cli
          cargo-edit
          cargo-audit
          cargo-outdated
          cargo-make
          (rust-bin.stable.latest.default.override {
            extensions = [
              "rust-src"
              "cargo"
              "rustc"
              "rust-analysis"
              "rustfmt"
            ];
          })
        ];
      in
      {
        devShell = pkgs.mkShell
          ({
            inherit buildInputs nativeBuildInputs;
            RUST_BACKTRACE = 1;
            shellHook = ''
              PGDIR=$PWD/storage/postgres
              export PGDATA=$PGDIR/data
              export PGHOST=$PGDIR/run
              export LOG_PATH=$PGDIR/LOG
              export PGDATABASE=saunf
              export DATABASE_URL="postgresql:///$PGDATABASE?host=$PGHOST"
              export SAUNF_DATABASE_URL=$DATABASE_URL

              if [ ! -d $PGHOST ]; then
                mkdir -p $PGHOST
              fi
              if [ ! -d $PGDATA ]; then
                echo 'Initializing postgresql database...'
                initdb $PGDATA --auth=trust >/dev/null
              fi
            '';
          } // buildEnvVars);
      });
}
