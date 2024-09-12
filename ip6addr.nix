{ mkDerivation, base, IPv6Addr, lib, optparse-applicative, text }:
mkDerivation {
  pname = "ip6addr";
  version = "2.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  doHaddock = false;
  executableHaskellDepends = [
    base IPv6Addr optparse-applicative text
  ];
  homepage = "https://github.com/MichelBoucey/ip6addr";
  description = "Commandline tool to deal with IPv6 address text representations";
  license = lib.licenses.bsd3;
  mainProgram = "ip6addr";
}
