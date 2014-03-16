import Control.Monad
import Data.ByteString.Char8 (pack, unpack, singleton)
import Data.Maybe
import System.Environment
import System.Exit
import System.Hardware.Serialport

helpMsg = unlines [ "Usage: powerstrip [OPTION] DEVICE COMMAND [PORT]..."
                  , "An interface for the USB controlled powerstrip written in Haskell"
                  , "Commands: up, down, status"
                  , "Ports   : 0, 1"
                  , ""
                  , "Options:"
                  , "  -h, --help       show this help message"
                  ]

data Command = Unpower | Power | Status deriving (Show, Eq, Enum)
data Port = Port0 | Port1 deriving (Show, Eq, Enum)
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

data Arguments = Arguments
    {   device  :: String
    ,   command :: Command
    ,   ports   :: [Port]
    } deriving (Show, Eq)

parseArgs :: [String] -> Either String Arguments
parseArgs (d:c:p) = if and [isJust mCmd, length p == length mPorts] then Right args else Left helpMsg
    where
        mCmd = parseCommand c
        mPorts = mapMaybe parsePort p
        args = Arguments d (fromJust mCmd) (if null mPorts then [Port0 ..] else mPorts)
parseArgs _ = Left helpMsg

main = do
    argv <- getArgs
    either putStrLn execute $ parseArgs argv

execute (Arguments d c p) = communicate d c p >>= (\x -> mapM_ (uncurry printStatus) $ zip p x)

printStatus p s = putStrLn $ "Socket " ++ (show . fromEnum) p ++ " is " ++ show s

communicate device cmd ports = withSerial device defaultSerialSettings (communicate' cmd ports)
communicate' cmd ports serial = do
    when (cmd /= Status) $ mapM_ (\x -> send serial $ pack $ decideByteString cmd x) ports
    out <- mapM (queryStatus . decideByteString Status) ports
    return $ map (parseStatus . head . unpack) out
    where
        decideByteString cmd port = show $ fromEnum cmd + (fromEnum port) * 3
        queryStatus c = send serial (pack c) >> recv serial 1
