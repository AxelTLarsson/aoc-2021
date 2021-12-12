{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [ ghc ormolu ];
}

# { nixpkgs ? import <nixpkgs> { }, compiler ? "ghc8107" }:

# (import ./default.nix { inherit nixpkgs compiler; }).env
