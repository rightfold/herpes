{ mkDerivation
, base
, free
, generics-sop
, gtk
, hashable
, kan-extensions
, lens
, mtl
, text
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
        kan-extensions
        lens
        mtl
        text
        unordered-containers
    ];
}
