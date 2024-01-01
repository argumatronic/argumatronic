{ mkDerivation, base, blaze-html, blaze-markup, containers
, doctemplates, filepath, hakyll, lib, pandoc, time
}:
mkDerivation {
  pname = "argumatronic";
  version = "0.1.0.0";
  src = ./argumatronic;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base blaze-html blaze-markup containers doctemplates filepath
    hakyll pandoc time
  ];
  license = "unknown";
  mainProgram = "site";
}
