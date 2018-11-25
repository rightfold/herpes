{ nixpkgs ? import ./nix/nixpkgs.nix {} }:
{
    herpes = nixpkgs.haskellPackages.callPackage ./herpes {};
}
