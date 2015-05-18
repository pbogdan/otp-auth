# otp-auth

otp-auth is a command line application for generating one-time
passwords used by two-factor authentication systems. The
implementation supports time-based one-time password (TOTP) algorithm
specified in RFC 6238, provided by the [OTP haskell library](
https://hackage.haskell.org/package/OTP)


## Installation

Building from source requires git, GHC and cabal:

```shell
git clone https://github.com/pbogdan/otp-auth.git
cd otp-auth
cabal install
```

make sure `$HOME/.cabal/bin` is in your `$PATH`.

## Usage

run

`otp-auth --help`

for available options.
