{-# LANGUAGE OverloadedStrings #-}

import           Data.Monoid ((<>))

import           Play

import           Options.Applicative
                        ( CommandFields, Mod, Parser, ParserInfo, ParserPrefs, (<**>)
                        , auto, option, help, helper, info, long, metavar)
import qualified Options.Applicative as O


main :: IO ()
main =
  O.customExecParser p opts >>= commandHandler
  where
    opts :: ParserInfo Command
    opts = info (helper <*> pCommand)
      ( O.fullDesc <> O.header "dist-play : A toy program that uses the distributed-process libray"
      )
    p :: ParserPrefs
    p = O.prefs O.showHelpOnEmpty

-- -----------------------------------------------------------------------------

data Command
  = RunCommand SendSeconds WaitSeconds (Maybe SeedValue)

pCommand :: Parser Command
pCommand = O.subparser $ mconcat
  [ subCommand "run"
      "Run the program."
      (RunCommand <$> sendSecondsP <*> waitSecondsP <*> seedValueP)
  ]
  where
    subCommand :: String -> String -> Parser a -> Mod CommandFields a
    subCommand label description parser =
      O.command label (info (parser <**> helper) (O.progDesc description))

sendSecondsP :: Parser SendSeconds
sendSecondsP = SendSeconds <$> option auto
  ( long "send-for"
  <> metavar "SEND_SECONDS"
  <> help "The number of seconds to send messages."
  )

waitSecondsP :: Parser WaitSeconds
waitSecondsP = WaitSeconds <$> option auto
  ( long "wait-for"
  <> metavar "WAIT_SECONDS"
  <> help "The number of seconds to wait for messages before printing results."
  )

seedValueP :: Parser (Maybe SeedValue)
seedValueP = fmap SeedValue <$> O.optional
  ( option auto
  $ long "with-seed"
  <> metavar "WITH_SEED"
  <> help "The optional random seed value."
  )

-- -----------------------------------------------------------------------------

commandHandler :: Command -> IO ()
commandHandler cmd =
  case cmd of
    RunCommand send wait mSeed ->
      putStrLn $ "run command with " ++ show (send, wait, mSeed)
