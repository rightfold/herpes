{ nixpkgs ? import ./nix/nixpkgs.nix {} }:
let
    root = import ./. { nixpkgs = nixpkgs; };
in
    {
        herpes = root.herpes.env.overrideAttrs (p: {
            nativeBuildInputs = p.nativeBuildInputs ++ [
                nixpkgs.haskellPackages.cabal-install
                nixpkgs.haskellPackages.ghcid
            ];
        });
    }
