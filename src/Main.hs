{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (when, forever)
import Data.ByteString (unpack)
import Data.ByteString.Base32 (decode)
import Data.ByteString.Char8 (pack)
import Data.Monoid (mempty)
import Data.OTP
import Data.Time
import Options.Applicative

data Opts = Opts
    { optsIsAsci :: Bool
    , optsLength :: Int
    , optsInterval :: Int
    , optsLoop :: Bool
    , optsSecret :: String
    }

parseOpts :: Parser Opts
parseOpts = Opts <$>
    flag
        False
        True
        (long "ascii" <>
         showDefault <>
         help
             "treat the secret key as plain text, rather than base32-encoded string (default: false)") <*>
    option
        auto
        (long "length" <>
         showDefault <>
         metavar "length" <>
         help "the length of password/token to generate" <>
         value 6) <*>
    option
        auto
        (long "interval" <>
         showDefault <>
         metavar "interval" <>
         help "refresh interval" <>
         value 30) <*>
    flag
        False
        True
        (long "loop" <>
         showDefault <>
         help "") <*>
    strOption
        (long "secret" <>
         showDefault <>
         metavar "secret" <>
         help "secret key")
        
main :: IO ()
main = do
    opts <- execParser (info (helper <*> parseOpts) (fullDesc <> mempty))
    generatePassword opts

generatePassword :: Opts -> IO ()
generatePassword opts = do
    let secret = pack $ optsSecret opts
    
    let decoder = if optsIsAsci opts then Right else decode
    
    case decoder secret of
      Right s -> do
          let bytes = unpack s
          time <- getCurrentTime
          putStrLn . padZeros (optsLength opts) $ totp bytes time (optsLength opts) (optsInterval opts)
          
          when (optsLoop opts) . forever $ do
              threadDelay (optsInterval opts * 1000000)
              newTime <- getCurrentTime
              putStrLn . padZeros (optsLength opts) $ totp bytes newTime (optsLength opts) (optsInterval opts)
              return ()
      Left e -> do
          print e
          return ()

padZeros :: Int -> Int -> String
padZeros len num = 
    let s = show num
    in
      replicate (len  - length s) '0' ++ s
