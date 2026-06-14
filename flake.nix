{
  description = "Norg parser library written in Rust";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {inherit system;};
        toolchain = pkgs.rustPlatform;
        cargoPackage = (pkgs.lib.importTOML "${self}/Cargo.toml").package;
      in rec {
        packages.default = toolchain.buildRustPackage {
          pname = cargoPackage.name;
          version = cargoPackage.version;
          src = pkgs.lib.cleanSource "${self}";
          cargoLock = {
            lockFile = "${self}/Cargo.lock";
            allowBuiltinFetchGit = true;
          };
          useNextest = true;
          dontUseCargoParallelTests = true;

          meta = {
            description = "Norg parser library written in Rust";
            license = pkgs.lib.licenses.gpl2Only;
          };
        };

        apps.default = flake-utils.lib.mkApp {drv = packages.default;};

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            (with toolchain; [
              cargo
              rustc
              rustLibSrc
            ])
            clippy
            rustfmt
            cargo-edit
            cargo-nextest
            rust-analyzer
          ];

          RUST_SRC_PATH = "${toolchain.rustLibSrc}";
        };
      }
    );
}
