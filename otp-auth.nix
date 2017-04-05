{ mkDerivation, base, base32-bytestring, binary, bytestring, hspec
, optparse-applicative, OTP, stdenv, time
}:
mkDerivation {
  pname = "otp-auth";
  version = "0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base base32-bytestring binary bytestring optparse-applicative OTP
    time
  ];
  testHaskellDepends = [ base hspec ];
  license = stdenv.lib.licenses.bsd3;
}
