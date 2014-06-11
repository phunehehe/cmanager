{ cabal, blazeHtml, filemanip, filepath, happstackLite
, happstackServer, HTTP, mtl, split, text, config
}:

cabal.mkDerivation (self: {
  pname = "cmanager";
  version = "0.1.0.0";
  src = "${config.cmanager.siteDir}";
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    blazeHtml filemanip filepath happstackLite happstackServer HTTP mtl
    split text
  ];
  meta = {
    homepage = "https://bitbucket.org/phunehehe/cmanager";
    description = "A web service that manages CGroups";
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
