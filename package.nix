{ cabal, blazeHtml, filemanip, filepath, happstackLite
, happstackServer, HTTP, mtl, split, text, config
}:

cabal.mkDerivation (self: {
  pname = "cgroups-manager";
  version = "0.1.0.0";
  src = "${config.cgroupsManager.siteDir}";
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    blazeHtml filemanip filepath happstackLite happstackServer HTTP mtl
    split text
  ];
  meta = {
    homepage = "https://bitbucket.org/phunehehe/cgroups-manager";
    description = "A web service that manages CGroups";
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
