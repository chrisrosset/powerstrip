{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad
import Data.ByteString.Char8 (pack, unpack, singleton)
import Data.Maybe
import System.Console.CmdArgs
import System.Exit
import System.Hardware.Serialport

_PROGRAM_NAME   = "Powerstrip.hs"
_PROGRAM_VER    = "0.1"
_PROGRAM_ABOUT  = "An interface for the USB controlled powerstrip written in Haskell"
_PROGRAM_AUTHOR = "Christopher Rosset 2014"

data StringOptions = StringOptions
    {   device  :: String
    ,   command :: String
    ,   ports   :: [String]
    } deriving (Data, Typeable, Show, Eq)

data Port = Port0 | Port1 deriving (Show, Eq, Enum)
data Command = Unpower | Power | Status deriving (Show, Eq, Enum)
data Status = Powered | Unpowered deriving (Show, Eq)

parsePort :: String -> Maybe Port
parsePort "0"   = Just Port0
parsePort "1"   = Just Port1
parsePort _     = Nothing

parseCommand :: String -> Maybe Command
parseCommand "up"     = Just Power
parseCommand "down"   = Just Unpower
parseCommand "status" = Just Status
parseCommand _        = Nothing

parseStatus :: Char -> Status
parseStatus c = if c == '0' then Unpowered else Powered

myProgOpts = cmdArgsMode $ StringOptions
    {   device  = def &= argPos 0
    ,   command = def &= argPos 1
    ,   ports   = def &= args
    }
    &= versionArg [explicit, name "version", name "v"]
    &= helpArg [explicit, name "help", name "h"]
    &= help    _PROGRAM_ABOUT
    &= program _PROGRAM_NAME
    &= summary (concat [_PROGRAM_NAME, " version ", _PROGRAM_VER])

main = do
    opts <- cmdArgsRun myProgOpts

    let maybeCmd = parseCommand $ command opts
    let portsList  = mapMaybe parsePort $ ports opts
    let portsList' = if null portsList then [Port0 ..] else portsList

    when (isNothing maybeCmd) $ putStrLn "invalid command" >> exitWith (ExitFailure 1)
    when (length portsList /= length (ports opts)) $ putStrLn "invalid port" >> exitWith (ExitFailure 1)

    stats <- communicate (device opts) (fromJust maybeCmd) portsList'
    mapM_ (uncurry printStatus) $ zip portsList' stats

printStatus p s = putStrLn $ "Socket " ++ (show . fromEnum) p ++ " is " ++ show s

communicate device cmd ports = withSerial device defaultSerialSettings (communicate' cmd ports)
communicate' cmd ports serial = do
    when (cmd /= Status) $ mapM_ (\x -> send serial $ pack $ decideByteString cmd x) ports
    out <- mapM (queryStatus . decideByteString Status) ports
    return $ map (parseStatus . head . unpack) out
    where
        decideByteString cmd port = show $ fromEnum cmd + (fromEnum port) * 3
        queryStatus c = send serial (pack c) >> recv serial 1
