rec {
  compiler = "";
  resolver = { nixpkgs ? import <nixpkgs> {}, compiler ? compiler }:
    let
      oldResolver = if compiler == "" then nixpkgs.haskellPackages else builtins.getAttr compiler nixpkgs.haskell.packages;
      callPackage = oldResolver.callPackage;

      overrideFunction = self: super: rec {
        HUnit = callPackage
          (
            { mkDerivation, base, deepseq, filepath, stdenv }:
            mkDerivation {
              pname = "HUnit";
              version = "1.3.1.1";
              sha256 = "1y4fv8r7xi302ahj6p71hvsgz3rkb2c4vw09j935ns5bj11grrck";
              libraryHaskellDepends = [ base deepseq ];
              testHaskellDepends = [ base deepseq filepath ];
              homepage = "https://github.com/hspec/HUnit#readme";
              description = "A unit testing framework for Haskell";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        ansi-terminal = callPackage
          (
            { mkDerivation, base, stdenv, unix }:
            mkDerivation {
              pname = "ansi-terminal";
              version = "0.6.2.3";
              sha256 = "0hpfw0k025y681m9ml1c712skrb1p4vh7z5x1f0ci9ww7ssjrh2d";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [ base unix ];
              executableHaskellDepends = [ base unix ];
              homepage = "https://github.com/feuerbach/ansi-terminal";
              description = "Simple ANSI terminal support, with Windows compatibility";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        base-compat = callPackage
          (
            { mkDerivation, base, hspec, QuickCheck, stdenv, unix }:
            mkDerivation {
              pname = "base-compat";
              version = "0.9.1";
              sha256 = "0jj6nq0vb8ap3724c3r3cavc298m1gm238vmgi7wzzxr8s0v8cqh";
              libraryHaskellDepends = [ base unix ];
              testHaskellDepends = [ base hspec QuickCheck ];
              description = "A compatibility layer for base";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hspec QuickCheck; };
        bindings-DSL = callPackage
          (
            { mkDerivation, base, stdenv }:
            mkDerivation {
              pname = "bindings-DSL";
              version = "1.0.23";
              sha256 = "1rqhkk8hn1xjl3705dvakxx93q89vp0fw22v2cbrlapbir27cv7b";
              libraryHaskellDepends = [ base ];
              homepage = "https://github.com/jwiegley/bindings-dsl/wiki";
              description = "FFI domain specific language, on top of hsc2hs";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        hspec-discover = callPackage
          (
            { mkDerivation, base, directory, filepath, hspec-meta, stdenv }:
            mkDerivation {
              pname = "hspec-discover";
              version = "2.2.3";
              sha256 = "0bx9nlc07vihkm0ykfz2fcwd5v6zszb1mw81mczi72k2mpbm6q6w";
              isLibrary = true;
              isExecutable = true;
              libraryHaskellDepends = [ base directory filepath ];
              executableHaskellDepends = [ base directory filepath ];
              testHaskellDepends = [ base directory filepath hspec-meta ];
              homepage = "http://hspec.github.io/";
              description = "Automatically discover and run Hspec tests";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        hspec-expectations = callPackage
          (
            { mkDerivation, base, HUnit, stdenv }:
            mkDerivation {
              pname = "hspec-expectations";
              version = "0.7.2";
              sha256 = "1w56jiqfyl237sr207gh3b0l8sr9layy0mdsgd5wknzb49mif6ip";
              libraryHaskellDepends = [ base HUnit ];
              homepage = "https://github.com/sol/hspec-expectations#readme";
              description = "Catchy combinators for HUnit";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit; };
        mtl = callPackage
          (
            { mkDerivation, base, stdenv, transformers }:
            mkDerivation {
              pname = "mtl";
              version = "2.2.1";
              sha256 = "1icdbj2rshzn0m1zz5wa7v3xvkf6qw811p4s7jgqwvx1ydwrvrfa";
              revision = "1";
              editedCabalFile = "4b5a800fe9edf168fc7ae48c7a3fc2aab6b418ac15be2f1dad43c0f48a494a3b";
              libraryHaskellDepends = [ base transformers ];
              homepage = "http://github.com/ekmett/mtl";
              description = "Monad classes, using functional dependencies";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        network = callPackage
          (
            { mkDerivation, base, bytestring, HUnit, stdenv, test-framework
            , test-framework-hunit, unix
            }:
            mkDerivation {
              pname = "network";
              version = "2.6.2.1";
              sha256 = "1yhvpd4wigz165jvyvw9zslx7lgqdj63jh3zv5s74b5ykdfa3zd3";
              libraryHaskellDepends = [ base bytestring unix ];
              testHaskellDepends = [
                base bytestring HUnit test-framework test-framework-hunit
              ];
              homepage = "https://github.com/haskell/network";
              description = "Low-level networking interface";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit; };
        primitive = callPackage
          (
            { mkDerivation, base, ghc-prim, stdenv, transformers }:
            mkDerivation {
              pname = "primitive";
              version = "0.6.1.0";
              sha256 = "1j1q7l21rdm8kfs93vibr3xwkkhqis181w2k6klfhx5g5skiywwk";
              revision = "1";
              editedCabalFile = "6ec7c2455c437aba71f856b797e7db440c83719509aa63a9a3d1b4652ca3683d";
              libraryHaskellDepends = [ base ghc-prim transformers ];
              testHaskellDepends = [ base ghc-prim ];
              homepage = "https://github.com/haskell/primitive";
              description = "Primitive memory-related operations";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        random = callPackage
          (
            { mkDerivation, base, stdenv, time }:
            mkDerivation {
              pname = "random";
              version = "1.1";
              sha256 = "0nis3lbkp8vfx8pkr6v7b7kr5m334bzb0fk9vxqklnp2aw8a865p";
              libraryHaskellDepends = [ base time ];
              testHaskellDepends = [ base ];
              description = "random number library";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        setenv = callPackage
          (
            { mkDerivation, base, stdenv, unix }:
            mkDerivation {
              pname = "setenv";
              version = "0.1.1.3";
              sha256 = "0cnbgrvb9byyahb37zlqrj05rj25v190crgcw8wmlgf0mwwxyn73";
              revision = "1";
              editedCabalFile = "c5916ac0d2a828473cd171261328a290afe0abd799db1ac8c310682fe778c45b";
              libraryHaskellDepends = [ base unix ];
              description = "A cross-platform library for setting environment variables";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        stm = callPackage
          (
            { mkDerivation, array, base, stdenv }:
            mkDerivation {
              pname = "stm";
              version = "2.4.4.1";
              sha256 = "111kpy1d6f5c0bggh6hyfm86q5p8bq1qbqf6dw2x4l4dxnar16cg";
              libraryHaskellDepends = [ array base ];
              description = "Software Transactional Memory";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        async = callPackage
          (
            { mkDerivation, base, HUnit, stdenv, stm, test-framework
            , test-framework-hunit
            }:
            mkDerivation {
              pname = "async";
              version = "2.1.0";
              sha256 = "0brcy9bxhy0kxwvh3sfahgd2bg3zgbkhm5nrikf5r2y6z48pdhwk";
              libraryHaskellDepends = [ base stm ];
              testHaskellDepends = [
                base HUnit test-framework test-framework-hunit
              ];
              homepage = "https://github.com/simonmar/async";
              description = "Run IO operations asynchronously and wait for their results";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit stm; };
        text = callPackage
          (
            { mkDerivation, array, base, binary, bytestring, deepseq, directory
            , ghc-prim, HUnit, integer-gmp, QuickCheck, quickcheck-unicode
            , random, stdenv, test-framework, test-framework-hunit
            , test-framework-quickcheck2
            }:
            mkDerivation {
              pname = "text";
              version = "1.2.2.1";
              sha256 = "0nrrzx0ws7pv4dx9jbc6jm2734al1cr0m6iwcnbck4v2yfyv3p8s";
              libraryHaskellDepends = [
                array base binary bytestring deepseq ghc-prim integer-gmp
              ];
              testHaskellDepends = [
                array base binary bytestring deepseq directory ghc-prim HUnit
                integer-gmp QuickCheck quickcheck-unicode random test-framework
                test-framework-hunit test-framework-quickcheck2
              ];
              doCheck = false;
              homepage = "https://github.com/bos/text";
              description = "An efficient packed Unicode text type";
              license = stdenv.lib.licenses.bsd3;
              doHaddock = false;
            }
          )
          { inherit HUnit QuickCheck random; };
        blaze-builder = callPackage
          (
            { mkDerivation, base, bytestring, deepseq, HUnit, QuickCheck
            , stdenv, test-framework, test-framework-hunit
            , test-framework-quickcheck2, text, utf8-string
            }:
            mkDerivation {
              pname = "blaze-builder";
              version = "0.4.0.1";
              sha256 = "1id3w33x9f7q5m3xpggmvzw03bkp94bpfyz81625bldqgf3yqdn1";
              libraryHaskellDepends = [ base bytestring deepseq text ];
              testHaskellDepends = [
                base bytestring HUnit QuickCheck test-framework
                test-framework-hunit test-framework-quickcheck2 text utf8-string
              ];
              homepage = "http://github.com/lpsmith/blaze-builder";
              description = "Efficient buffered output";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit QuickCheck text; };
        hashable = callPackage
          (
            { mkDerivation, base, bytestring, ghc-prim, HUnit, integer-gmp
            , QuickCheck, random, stdenv, test-framework, test-framework-hunit
            , test-framework-quickcheck2, text, unix
            }:
            mkDerivation {
              pname = "hashable";
              version = "1.2.4.0";
              sha256 = "1wrwpchksxd1i63ydpqy6jkdzxrb5qsy64rfiv9lik9r1kdp35pv";
              libraryHaskellDepends = [
                base bytestring ghc-prim integer-gmp text
              ];
              testHaskellDepends = [
                base bytestring ghc-prim HUnit QuickCheck random test-framework
                test-framework-hunit test-framework-quickcheck2 text unix
              ];
              homepage = "http://github.com/tibbe/hashable";
              description = "A class for types that can be converted to a hash value";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit QuickCheck random text; };
        tf-random = callPackage
          (
            { mkDerivation, base, primitive, random, stdenv, time }:
            mkDerivation {
              pname = "tf-random";
              version = "0.5";
              sha256 = "0445r2nns6009fmq0xbfpyv7jpzwv0snccjdg7hwj4xk4z0cwc1f";
              libraryHaskellDepends = [ base primitive random time ];
              description = "High-quality splittable pseudorandom number generator";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit primitive random; };
        QuickCheck = callPackage
          (
            { mkDerivation, base, containers, random, stdenv, template-haskell
            , test-framework, tf-random, transformers
            }:
            mkDerivation {
              pname = "QuickCheck";
              version = "2.8.2";
              sha256 = "1ai6k5v0bibaxq8xffcblc6rwmmk6gf8vjyd9p2h3y6vwbhlvilq";
              libraryHaskellDepends = [
                base containers random template-haskell tf-random transformers
              ];
              testHaskellDepends = [
                base containers template-haskell test-framework
              ];
              homepage = "https://github.com/nick8325/quickcheck";
              description = "Automatic testing of Haskell programs";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit random tf-random; };
        quickcheck-io = callPackage
          (
            { mkDerivation, base, HUnit, QuickCheck, stdenv }:
            mkDerivation {
              pname = "quickcheck-io";
              version = "0.1.2";
              sha256 = "1kf1kfw9fsmly0rvzvdf6jvdw10qhkmikyj0wcwciw6wad95w9sh";
              libraryHaskellDepends = [ base HUnit QuickCheck ];
              homepage = "https://github.com/hspec/quickcheck-io#readme";
              description = "Use HUnit assertions as QuickCheck properties";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit QuickCheck; };
        hspec-core = callPackage
          (
            { mkDerivation, ansi-terminal, async, base, deepseq
            , hspec-expectations, hspec-meta, HUnit, process, QuickCheck
            , quickcheck-io, random, setenv, silently, stdenv, tf-random, time
            , transformers
            }:
            mkDerivation {
              pname = "hspec-core";
              version = "2.2.3";
              sha256 = "0llnr7gg1xa1l8jz9ivhjq7q12773x2i5xp3wlyyvq0sj9cnkyh1";
              libraryHaskellDepends = [
                ansi-terminal async base deepseq hspec-expectations HUnit
                QuickCheck quickcheck-io random setenv tf-random time transformers
              ];
              testHaskellDepends = [
                ansi-terminal async base deepseq hspec-expectations hspec-meta
                HUnit process QuickCheck quickcheck-io random setenv silently
                tf-random time transformers
              ];
              homepage = "http://hspec.github.io/";
              description = "A Testing Framework for Haskell";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit ansi-terminal async hspec-expectations HUnit QuickCheck quickcheck-io random setenv tf-random; };
        hspec = callPackage
          (
            { mkDerivation, base, directory, hspec-core, hspec-discover
            , hspec-expectations, hspec-meta, HUnit, QuickCheck, stdenv
            , stringbuilder, transformers
            }:
            mkDerivation {
              pname = "hspec";
              version = "2.2.3";
              sha256 = "0432dxkxrmsvz78g8inwhklid677161yp8r0pw8cd1bdx179j7ji";
              libraryHaskellDepends = [
                base hspec-core hspec-discover hspec-expectations HUnit QuickCheck
                transformers
              ];
              testHaskellDepends = [
                base directory hspec-core hspec-meta stringbuilder
              ];
              homepage = "http://hspec.github.io/";
              description = "A Testing Framework for Haskell";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hspec-core hspec-discover hspec-expectations HUnit QuickCheck; };
        transformers-compat = callPackage
          (
            { mkDerivation, base, ghc-prim, stdenv, transformers }:
            mkDerivation {
              pname = "transformers-compat";
              version = "0.5.1.4";
              sha256 = "17yam0199fh9ndsn9n69jx9nvbsmymzzwbi23dck3dk4q57fz0fq";
              libraryHaskellDepends = [ base ghc-prim transformers ];
              homepage = "http://github.com/ekmett/transformers-compat/";
              description = "A small compatibility shim exposing the new types from transformers 0.3 and 0.4 to older Haskell platforms.";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { };
        exceptions = callPackage
          (
            { mkDerivation, base, mtl, QuickCheck, stdenv, stm
            , template-haskell, test-framework, test-framework-quickcheck2
            , transformers, transformers-compat
            }:
            mkDerivation {
              pname = "exceptions";
              version = "0.8.2.1";
              sha256 = "0d8hhmnryn80cnal1r1p5323v7y8z35vny0cwmaihjphy9zqfdf4";
              libraryHaskellDepends = [
                base mtl stm template-haskell transformers transformers-compat
              ];
              testHaskellDepends = [
                base mtl QuickCheck stm template-haskell test-framework
                test-framework-quickcheck2 transformers transformers-compat
              ];
              homepage = "http://github.com/ekmett/exceptions/";
              description = "Extensible optionally-pure exceptions";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit mtl QuickCheck stm transformers-compat; };
        mmorph = callPackage
          (
            { mkDerivation, base, mtl, stdenv, transformers
            , transformers-compat
            }:
            mkDerivation {
              pname = "mmorph";
              version = "1.0.6";
              sha256 = "1i8dzrc5qi3ryc9vrrmpn3sihmramsbhhd592w4w2k5g26qr3hql";
              libraryHaskellDepends = [
                base mtl transformers transformers-compat
              ];
              description = "Monad morphisms";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit mtl transformers-compat; };
        transformers-base = callPackage
          (
            { mkDerivation, base, stdenv, stm, transformers
            , transformers-compat
            }:
            mkDerivation {
              pname = "transformers-base";
              version = "0.4.4";
              sha256 = "11r3slgpgpra6zi2kjg3g60gvv17b1fh6qxipcpk8n86qx7lk8va";
              revision = "1";
              editedCabalFile = "fb1a305f29cbf6ac182af7e67efaae9fcb9664d8d9606bb8a7f3414ad4c8d7a4";
              libraryHaskellDepends = [
                base stm transformers transformers-compat
              ];
              homepage = "https://github.com/mvv/transformers-base";
              description = "Lift computations from the bottom of a transformer stack";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit stm transformers-compat; };
        monad-control = callPackage
          (
            { mkDerivation, base, stdenv, stm, transformers, transformers-base
            , transformers-compat
            }:
            mkDerivation {
              pname = "monad-control";
              version = "1.0.0.5";
              sha256 = "1ya3i7x7p643fbrylf6bh8731mjvgn067qw2h8ijqbn805bp2pq5";
              libraryHaskellDepends = [
                base stm transformers transformers-base transformers-compat
              ];
              homepage = "https://github.com/basvandijk/monad-control";
              description = "Lift control operations, like exception catching, through monad transformers";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit stm transformers-base transformers-compat; };
        lifted-base = callPackage
          (
            { mkDerivation, base, HUnit, monad-control, stdenv, test-framework
            , test-framework-hunit, transformers, transformers-base
            , transformers-compat
            }:
            mkDerivation {
              pname = "lifted-base";
              version = "0.2.3.6";
              sha256 = "1yz14a1rsgknwyl08n4kxrlc26hfwmb95a3c2drbnsgmhdyq7iap";
              libraryHaskellDepends = [ base monad-control transformers-base ];
              testHaskellDepends = [
                base HUnit monad-control test-framework test-framework-hunit
                transformers transformers-base transformers-compat
              ];
              homepage = "https://github.com/basvandijk/lifted-base";
              description = "lifted IO operations from the base library";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit monad-control transformers-base transformers-compat; };
        resourcet = callPackage
          (
            { mkDerivation, base, containers, exceptions, hspec, lifted-base
            , mmorph, monad-control, mtl, stdenv, transformers
            , transformers-base, transformers-compat
            }:
            mkDerivation {
              pname = "resourcet";
              version = "1.1.7.3";
              sha256 = "1gz140ffsd61vsc1bn0sxjl337x2xghfh0f8c5p1af5vga4mmk7w";
              libraryHaskellDepends = [
                base containers exceptions lifted-base mmorph monad-control mtl
                transformers transformers-base transformers-compat
              ];
              testHaskellDepends = [ base hspec lifted-base transformers ];
              homepage = "http://github.com/snoyberg/conduit";
              description = "Deterministic allocation and freeing of scarce resources";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit exceptions hspec lifted-base mmorph monad-control mtl transformers-base transformers-compat; };
        conduit = callPackage
          (
            { mkDerivation, base, containers, exceptions, hspec, lifted-base
            , mmorph, mtl, QuickCheck, resourcet, safe, stdenv, transformers
            , transformers-base
            }:
            mkDerivation {
              pname = "conduit";
              version = "1.2.6.4";
              sha256 = "1wvkfnsb05swn5dbysxr2vwibqic6sjjhjbidwcsigqfb03y9i8z";
              libraryHaskellDepends = [
                base exceptions lifted-base mmorph mtl resourcet transformers
                transformers-base
              ];
              testHaskellDepends = [
                base containers exceptions hspec mtl QuickCheck resourcet safe
                transformers
              ];
              homepage = "http://github.com/snoyberg/conduit";
              description = "Streaming data processing library";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit exceptions hspec lifted-base mmorph mtl QuickCheck resourcet transformers-base; };
        lzma-conduit = callPackage
          (
            { mkDerivation, base, bindings-DSL, bytestring, conduit, HUnit
            , lzma, QuickCheck, resourcet, stdenv, test-framework
            , test-framework-hunit, test-framework-quickcheck2, transformers
            }:
            mkDerivation {
              pname = "lzma-conduit";
              version = "1.1.3.0";
              sha256 = "03rrxxd2h93civbd72lh41xhfziylyknyyi34dcxifx2aahfgydb";
              libraryHaskellDepends = [
                base bindings-DSL bytestring conduit resourcet transformers
              ];
              librarySystemDepends = [ lzma ];
              testHaskellDepends = [
                base bytestring conduit HUnit QuickCheck resourcet test-framework
                test-framework-hunit test-framework-quickcheck2
              ];
              homepage = "http://github.com/alphaHeavy/lzma-conduit";
              description = "Conduit interface for lzma/xz compression";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit bindings-DSL conduit HUnit QuickCheck resourcet; inherit (nixpkgs) lzma; };
        vector = callPackage
          (
            { mkDerivation, base, deepseq, ghc-prim, primitive, QuickCheck
            , random, stdenv, template-haskell, test-framework
            , test-framework-quickcheck2, transformers
            }:
            mkDerivation {
              pname = "vector";
              version = "0.11.0.0";
              sha256 = "1r1jlksy7b0kb0fy00g64isk6nyd9wzzdq31gx5v1wn38knj0lqa";
              revision = "1";
              editedCabalFile = "dfdf3252519ff35da59f977b7d37d6c5a6660673ce1234899af0111f7ece9c66";
              libraryHaskellDepends = [ base deepseq ghc-prim primitive ];
              testHaskellDepends = [
                base QuickCheck random template-haskell test-framework
                test-framework-quickcheck2 transformers
              ];
              homepage = "https://github.com/haskell/vector";
              description = "Efficient Arrays";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit primitive QuickCheck random; };
        binary-conduit = callPackage
          (
            { mkDerivation, base, binary, bytestring, conduit, hspec
            , QuickCheck, quickcheck-assertions, resourcet, stdenv, vector
            }:
            mkDerivation {
              pname = "binary-conduit";
              version = "1.2.3";
              sha256 = "0ymhxyf754j1pki7ap2vay8f9j49rzsjzp5yr253sn5wpw3qg8fr";
              revision = "1";
              editedCabalFile = "9cb1c58171575429a65947d1ef7e22c1501b4ca9becc07904c7b4a5066345e29";
              libraryHaskellDepends = [
                base binary bytestring conduit resourcet vector
              ];
              testHaskellDepends = [
                base binary bytestring conduit hspec QuickCheck
                quickcheck-assertions resourcet
              ];
              homepage = "http://github.com/qnikst/binary-conduit/";
              description = "data serialization/deserialization conduit library";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit conduit hspec QuickCheck resourcet vector; };
        scientific = callPackage
          (
            { mkDerivation, base, binary, bytestring, containers, deepseq
            , ghc-prim, hashable, integer-gmp, QuickCheck, smallcheck, stdenv
            , tasty, tasty-ant-xml, tasty-hunit, tasty-quickcheck
            , tasty-smallcheck, text, vector
            }:
            mkDerivation {
              pname = "scientific";
              version = "0.3.4.6";
              sha256 = "10pk3l32iqr88pad2ijz5050jiqsjzk16w8dygssxkkrndr5rldx";
              libraryHaskellDepends = [
                base binary bytestring containers deepseq ghc-prim hashable
                integer-gmp text vector
              ];
              testHaskellDepends = [
                base binary bytestring QuickCheck smallcheck tasty tasty-ant-xml
                tasty-hunit tasty-quickcheck tasty-smallcheck text
              ];
              homepage = "https://github.com/basvandijk/scientific";
              description = "Numbers represented using scientific notation";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit hashable QuickCheck text vector; };
        attoparsec = callPackage
          (
            { mkDerivation, array, base, bytestring, containers, deepseq
            , QuickCheck, quickcheck-unicode, scientific, stdenv
            , test-framework, test-framework-quickcheck2, text, transformers
            , vector
            }:
            mkDerivation {
              pname = "attoparsec";
              version = "0.13.0.1";
              sha256 = "0cprkr7bl4lrr80pz8mryb4rbfwdgpsrl7g0fbcaybhl8p5hm26f";
              libraryHaskellDepends = [
                array base bytestring containers deepseq scientific text
                transformers
              ];
              testHaskellDepends = [
                array base bytestring containers deepseq QuickCheck
                quickcheck-unicode scientific test-framework
                test-framework-quickcheck2 text transformers vector
              ];
              homepage = "https://github.com/bos/attoparsec";
              description = "Fast combinator parsing for bytestrings and text";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit QuickCheck scientific text vector; };
        zlib = callPackage
          (
            { mkDerivation, base, bytestring, HUnit, QuickCheck, stdenv, tasty
            , tasty-hunit, tasty-quickcheck, zlib
            }:
            mkDerivation {
              pname = "zlib";
              version = "0.6.1.1";
              sha256 = "0dd79dxf56d8f6ad9if3j87s9gg7yd17ckypjxwplrbkahlb9xf5";
              revision = "3";
              editedCabalFile = "b5fff98d06289c9c16ab78d994f88f18345ccf7d0ef3e5334bb417d763b07046";
              libraryHaskellDepends = [ base bytestring ];
              librarySystemDepends = [ zlib ];
              testHaskellDepends = [
                base bytestring HUnit QuickCheck tasty tasty-hunit tasty-quickcheck
              ];
              description = "Compression and decompression in the gzip and zlib formats";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit QuickCheck; inherit (nixpkgs) zlib; };
        streaming-commons = callPackage
          (
            { mkDerivation, array, async, base, blaze-builder, bytestring
            , deepseq, directory, hspec, network, process, QuickCheck, random
            , stdenv, stm, text, transformers, unix, zlib
            }:
            mkDerivation {
              pname = "streaming-commons";
              version = "0.1.15.2";
              sha256 = "17a9gnkcwgb9rr2kv413ash4gb45dhhpfz1ns60w1rwcc1ad22i3";
              libraryHaskellDepends = [
                array async base blaze-builder bytestring directory network process
                random stm text transformers unix zlib
              ];
              testHaskellDepends = [
                array async base blaze-builder bytestring deepseq hspec network
                QuickCheck text unix zlib
              ];
              homepage = "https://github.com/fpco/streaming-commons";
              description = "Common lower-level functions needed by various streaming data libraries";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit async blaze-builder hspec network QuickCheck random stm text zlib; };
        conduit-extra = callPackage
          (
            { mkDerivation, async, attoparsec, base, blaze-builder, bytestring
            , bytestring-builder, conduit, directory, exceptions, filepath
            , hspec, monad-control, network, primitive, process, resourcet
            , stdenv, stm, streaming-commons, text, transformers
            , transformers-base
            }:
            mkDerivation {
              pname = "conduit-extra";
              version = "1.1.11";
              sha256 = "0bzac4cf24r4lgnvi06zvm63hjbjcsg7zzmmz8glvmqnbgwhj037";
              libraryHaskellDepends = [
                attoparsec base blaze-builder bytestring conduit directory
                exceptions filepath monad-control network primitive process
                resourcet stm streaming-commons text transformers transformers-base
              ];
              testHaskellDepends = [
                async attoparsec base blaze-builder bytestring bytestring-builder
                conduit exceptions hspec process resourcet stm streaming-commons
                text transformers transformers-base
              ];
              homepage = "http://github.com/snoyberg/conduit";
              description = "Batteries included conduit: adapters for common libraries";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit async attoparsec blaze-builder conduit exceptions hspec monad-control network primitive resourcet stm streaming-commons text transformers-base; };
      };

      newResolver = oldResolver.override {
        overrides = overrideFunction;
      };

    in newResolver;
}
