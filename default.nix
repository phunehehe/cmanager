{ cabal, aeson, blazeHtml, filemanip, filepath, happstackLite, HTTP
, mtl, split, text
}:

cabal.mkDerivation (self: {
  pname = "cmanager";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson blazeHtml filemanip filepath happstackLite HTTP mtl split
    text
  ];
  meta = {
    homepage = "https://bitbucket.org/phunehehe/cmanager";
    description = "A web service that manages CGroups";
    license = "unknown";
    platforms = self.ghc.meta.platforms;
  };
})
