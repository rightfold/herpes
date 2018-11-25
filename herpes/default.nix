{ mkDerivation
, base
, free
, generics-sop
, kan-extensions
, text }:
mkDerivation {
    pname = "wla";
    version = "0.0.0.0";
    license = null;
    src = ./.;
    buildDepends = [
        base
        free
        generics-sop
        kan-extensions
        text
    ];
}
