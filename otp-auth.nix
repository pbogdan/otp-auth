{ mkDerivation, base, binary, bytestring, hspec
, optparse-applicative, OTP, sandi, stdenv, time
}:
mkDerivation {
  pname = "otp-auth";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  doCheck = false;
  executableHaskellDepends = [
    base binary bytestring optparse-applicative OTP sandi time
  ];
  testHaskellDepends = [ base hspec ];
  license = stdenv.lib.licenses.bsd3;
}
