{ cabal, blazeHtml, filemanip, filepath, happstackLite
, happstackServer, HTTP, mtl, split, text
}:

cabal.mkDerivation (self: {
  pname = "cgroups-manager";
  version = "0.1.0.0";
  src = "/cgroups/cgroups-manager.tar.gz";
  sha256 = "09817266e54a74dfcc07289204efce4bd87f575744355e4447adc7e6890805f7";
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
