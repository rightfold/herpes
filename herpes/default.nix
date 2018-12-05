{ mkDerivation
, base
, free
, generics-sop
, gtk
, hashable
, haskell-src-exts
, kan-extensions
, lens
, mtl
, text
, thyme
, unordered-containers }:
mkDerivation {
    pname = "wla";
    version = "0.0.0.0";
    license = null;
    src = ./.;
    buildDepends = [
        base
        free
        generics-sop
        gtk
        hashable
        haskell-src-exts
        kan-extensions
        lens
        mtl
        text
        thyme
        unordered-containers
    ];
}
