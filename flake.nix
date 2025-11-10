{
  inputs = {
    nixpkgs.url      = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url  = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };
  outputs = { self, nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
        rustpkg = pkgs.rust-bin.selectLatestNightlyWith (toolchain: toolchain.default.override {
          extensions = [ "rust-src" "rust-analyzer" "rustfmt" "rustc-dev" "llvm-tools-preview" ];
          targets = [ "x86_64-unknown-linux-gnu" ];
        });
      in {
        devShells.default = with pkgs; mkShell rec {
          nativeBuildInputs = [
            pkg-config
            rustpkg
          ];
          LD_LIBRARY_PATH = "${lib.makeLibraryPath nativeBuildInputs}";
          RUST_BACKTRACE = 1;
          RUSTFLAGS = "-Zproc-macro-backtrace";
        };
      }
    );
}
