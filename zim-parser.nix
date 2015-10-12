{ mkDerivation, array, base, binary, binary-conduit, bytestring
, conduit, conduit-extra, hspec, lzma-conduit, resourcet, stdenv
}:
mkDerivation {
  pname = "zim-parser";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    array base binary binary-conduit bytestring conduit conduit-extra
    lzma-conduit resourcet
  ];
  testHaskellDepends = [
    array base binary binary-conduit bytestring conduit conduit-extra
    hspec lzma-conduit resourcet
  ];
  homepage = "https://github.com/robbinch/zim-parser#readme";
  description = "Read and parse ZIM files";
  license = stdenv.lib.licenses.gpl3;
}
