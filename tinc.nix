{ nixpkgs }:
rec {
  compiler = nixpkgs.haskellPackages;
  resolver =
    let
      callPackage = compiler.callPackage;

      overrideFunction = self: super: rec {
        HUnit = callPackage
          (
            { mkDerivation, base, deepseq, filepath, stdenv }:
            mkDerivation {
              pname = "HUnit";
              version = "1.3.1.2";
              sha256 = "10akdh4fl615rrshxi3m5gf414il1q42z4zqyb6q4jasmscvzpms";
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
              libraryHaskellDepends = [ base unix ];
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
        hspec-discover = callPackage
          (
            { mkDerivation, base, directory, filepath, hspec-meta, stdenv }:
            mkDerivation {
              pname = "hspec-discover";
              version = "2.2.4";
              sha256 = "1bz7wb8v0bx1amiz4bpj34xq97d1ia29n3f654wcrh6lacydp3dv";
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
        lzma = callPackage
          (
            { mkDerivation, base, bytestring, HUnit, lzma, QuickCheck, stdenv
            , tasty, tasty-hunit, tasty-quickcheck
            }:
            mkDerivation {
              pname = "lzma";
              version = "0.0.0.3";
              sha256 = "0i416gqi8j55nd1pqbkxvf3f6hn6fjys6gq98lkkxphva71j30xg";
              libraryHaskellDepends = [ base bytestring ];
              librarySystemDepends = [ lzma ];
              testHaskellDepends = [
                base bytestring HUnit QuickCheck tasty tasty-hunit tasty-quickcheck
              ];
              homepage = "https://github.com/hvr/lzma";
              description = "LZMA/XZ compression and decompression";
              license = stdenv.lib.licenses.bsd3;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit HUnit QuickCheck; inherit (nixpkgs) lzma; };
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
            { mkDerivation, base, bytestring, doctest, HUnit, stdenv
            , test-framework, test-framework-hunit, unix
            }:
            mkDerivation {
              pname = "network";
              version = "2.6.3.1";
              sha256 = "1rl2gl37cf4k0ddsq93q15fwdz1l25nhl4w205krbh7d5dg5y12p";
              libraryHaskellDepends = [ base bytestring unix ];
              testHaskellDepends = [
                base bytestring doctest HUnit test-framework test-framework-hunit
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
              version = "0.4.0.2";
              sha256 = "1m33y6p5xldni8p4fzg8fmsyqvkfmnimdamr1xjnsmgm3dkf9lws";
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
            , test-framework, tf-random, transformers, semigroups
            }:
            mkDerivation {
              pname = "QuickCheck";
              version = "2.9.2";
              sha256 = "119np67qvx8hyp9vkg4gr2wv3lj3j6ay2vl4hxspkg43ymb1cp0m";
              libraryHaskellDepends = [
                base containers random template-haskell tf-random transformers semigroups
              ];
              testHaskellDepends = [
                base containers template-haskell test-framework semigroups
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
              version = "0.1.3";
              sha256 = "1d68fcb9cx1bk8yzq28d4hbwjwj4y5y0kldd1nxlq7n54r75i66p";
              revision = "1";
              editedCabalFile = "9c0af3d194aa2d469c4cde8e26ad6566af32685face8ddb17919960f424c357a";
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
              version = "2.2.4";
              sha256 = "0x845ngfl6vf65fnpb5mm3wj0ql45pz11bnm0x4gxc4ybd9c52ij";
              revision = "1";
              editedCabalFile = "9a0c9fc612eb71ee55ebcaacbce010b87ffef8a535ed6ee1f50d8bd952dc86c3";
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
              version = "2.2.4";
              sha256 = "1cf90gqvg1iknja6ymxqxyabpahcxni3blqllh81ywbir3whljvj";
              revision = "1";
              editedCabalFile = "eb22cb737adc3312b21699b6ac4137489590ada1ee9ee9ae21aae3c342b3880f";
              libraryHaskellDepends = [
                base hspec-core hspec-discover hspec-expectations HUnit QuickCheck
                transformers
              ];
              testHaskellDepends = [
                base directory hspec-core hspec-discover hspec-expectations
                hspec-meta HUnit QuickCheck stringbuilder transformers
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
              version = "0.8.3";
              sha256 = "1gl7xzffsqmigam6zg0jsglncgzxqafld2p6kb7ccp9xirzdjsjd";
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
              version = "1.0.1.0";
              sha256 = "1x018gi5irznx5rgzmkr2nrgh26r8cvqwkcfc6n6y05pdjf21c6l";
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
              version = "0.2.3.8";
              sha256 = "17yz4n7q96x4cp8vxai8csn2vmpigxvipkfh48arahf91f0xy18n";
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
              version = "1.1.7.5";
              sha256 = "0nj0gwfd05divpdn7m47gy6bpcrwn3zk81gc303k0smrbqi0xlq5";
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
              version = "1.2.7";
              sha256 = "1r9vxpbcy441niw80byg68gkkn2xrqbz8l6x1q4d70fgassivcin";
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
              version = "1.2.4.1";
              sha256 = "10nalqf3zhg49b5drhw4y8zv9c3nsnlbc7bvw9la8vgzpihbnp24";
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
              version = "0.3.4.9";
              sha256 = "1a0q15kq0pk3pabxh536wgphh8713hhn8n55gm6s1y8a5dk310qh";
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
            , QuickCheck, quickcheck-unicode, scientific, stdenv, tasty
            , tasty-quickcheck, text, transformers, vector, fail, semigroups
            }:
            mkDerivation {
              pname = "attoparsec";
              version = "0.13.1.0";
              sha256 = "0r1zrrkbqv8w4pb459fj5izd1h85p9nrsp3gyzj7qiayjpa79p2j";
              libraryHaskellDepends = [
                array base bytestring containers deepseq scientific text
                transformers fail semigroups
              ];
              testHaskellDepends = [
                array base bytestring deepseq QuickCheck quickcheck-unicode
                scientific tasty tasty-quickcheck text transformers vector
                fail semigroups
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
              version = "0.1.16";
              sha256 = "0vhhm0z88b1r6s50bskdfh73acwfypm614nycmi9jwiyh84zbz8p";
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
            , hspec, monad-control, network, primitive, process, QuickCheck
            , resourcet, stdenv, stm, streaming-commons, text, transformers
            , transformers-base
            }:
            mkDerivation {
              pname = "conduit-extra";
              version = "1.1.13.3";
              sha256 = "0j3cqpkrn7lbpviv6w0gjh93fjjbh1an2sq0yz7svaawja8civy2";
              libraryHaskellDepends = [
                async attoparsec base blaze-builder bytestring conduit directory
                exceptions filepath monad-control network primitive process
                resourcet stm streaming-commons text transformers transformers-base
              ];
              testHaskellDepends = [
                async attoparsec base blaze-builder bytestring bytestring-builder
                conduit exceptions hspec process QuickCheck resourcet stm
                streaming-commons text transformers transformers-base
              ];
              homepage = "http://github.com/snoyberg/conduit";
              description = "Batteries included conduit: adapters for common libraries";
              license = stdenv.lib.licenses.mit;
              doCheck = false;
              doHaddock = false;
            }
          )
          { inherit async attoparsec blaze-builder conduit exceptions hspec monad-control network primitive QuickCheck resourcet stm streaming-commons text transformers-base; };
      };

      newResolver = compiler.override {
        overrides = overrideFunction;
      };

    in newResolver;
}
