{ mkDerivation, base, cmdargs, IPv6Addr, lib, text }:
mkDerivation {
  pname = "ip6addr";
  version = "1.0.4";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base cmdargs IPv6Addr text ];
  homepage = "https://github.com/MichelBoucey/ip6addr";
  description = "Commandline tool to deal with IPv6 address text representations";
  license = lib.licenses.bsd3;
  mainProgram = "ip6addr";
}
