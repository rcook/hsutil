#!/usr/bin/env stack
{-
    stack --resolver=lts-12.6 script
        --package hspec
        --package process
        --package temporary
-}

{-# OPTIONS_GHC -Wall -Werror #-}

-- Inspired by
-- https://stackoverflow.com/questions/8820903/haskell-how-to-timeout-a-function-that-runs-an-external-command
-- https://code.i-harness.com/en/q/8698a7

module Timeout (main) where

import Control.Exception (bracketOnError)
import Control.Monad (void)
import GHC.IO.Exception (IOErrorType(..))
import System.Exit (ExitCode(..))
import System.IO
    ( BufferMode(..)
    , Handle
    , hClose
    , hGetLine
    , hPutStrLn
    , hSetBuffering
    )
import System.IO.Error (mkIOError)
import System.IO.Temp (withSystemTempFile)
import System.Process
    ( CreateProcess(..)
    , ProcessHandle
    , StdStream(..)
    , createProcess
    , proc
    , terminateProcess
    , waitForProcess
    )
import System.Timeout (timeout)
import Text.Printf (printf)

import Test.Hspec

type ProcessAction a = (Handle, Handle, Handle, ProcessHandle) -> IO a

newtype Timeout = Timeout { seconds :: Int }

shell :: String
shell = "bash"

fromSeconds :: Int -> Timeout
fromSeconds = Timeout

timeoutProcess :: Timeout -> IO a -> IO (Maybe a)
timeoutProcess t = timeout (seconds t * 1000000)

requireHandles :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> (Handle, Handle, Handle, ProcessHandle)
requireHandles (Just hIn, Just hOut, Just hErr, ph) = (hIn, hOut, hErr, ph)
requireHandles _ = error "Handles required"

withProcess :: String -> [String] -> ProcessAction a -> IO (ExitCode, a)
withProcess command args f = bracketOnError
    (requireHandles <$> createProcess (proc command args)
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        })

    (\(_, _, _, ph) -> terminateProcess ph >> waitForProcess ph)

    (\(hIn, hOut, hErr, ph) -> do
        result <- f (hIn, hOut, hErr, ph)
        exitCode <- waitForProcess ph
        hClose hIn
        hClose hOut
        return (exitCode, result))

withCheckedProcess :: String -> [String] -> ProcessAction a -> IO a
withCheckedProcess command args f = do
    (exitCode, result) <- withProcess command args f
    case exitCode of
        ExitSuccess -> return result
        ExitFailure status ->
            ioError (mkIOError OtherError (printf "Process exited with status %d" status) Nothing Nothing)

main :: IO ()
main = hspec $ do
    describe "timeoutProcess and withProcess" $ do
        it "returns success when process returns 0 and action returns unit" $
            withTestScript $ \scriptPath -> do
                mbResult <- timeoutProcess (fromSeconds 1) $ withProcess shell [scriptPath, "0"] $ \(hIn, hOut, _, _) -> do
                    hSetBuffering hIn NoBuffering
                    hSetBuffering hOut NoBuffering
                    hPutStrLn hIn "SOMEINPUT"
                    hPutStrLn hIn "QUIT"
                    s0 <- hGetLine hOut
                    s0 `shouldBe` "INPUT: SOMEINPUT"
                    s1 <- hGetLine hOut
                    s1 `shouldBe` "INPUT: QUIT"
                    s2 <- hGetLine hOut
                    s2 `shouldBe` "echo-script exiting"
                    return ()
                mbResult `shouldBe` Just (ExitSuccess, ())
        it "returns success when process returns 0 and action returns string" $
            withTestScript $ \scriptPath -> do
                mbResult <- timeoutProcess (fromSeconds 1) $ withProcess shell [scriptPath, "0"] $ \(hIn, hOut, _, _) -> do
                    hSetBuffering hIn NoBuffering
                    hSetBuffering hOut NoBuffering
                    hPutStrLn hIn "SOMEINPUT"
                    hPutStrLn hIn "QUIT"
                    void $ hGetLine hOut
                    void $ hGetLine hOut
                    return "SOMERESULT"
                mbResult `shouldBe` Just (ExitSuccess, "SOMERESULT")
        it "returns failure when process returns non-0" $
            withTestScript $ \scriptPath -> do
                mbResult <- timeoutProcess (fromSeconds 1) $ withProcess shell [scriptPath, "1"] $ \(hIn, hOut, _, _) -> do
                    hSetBuffering hIn NoBuffering
                    hSetBuffering hOut NoBuffering
                    hPutStrLn hIn "SOMEINPUT"
                    hPutStrLn hIn "QUIT"
                    void $ hGetLine hOut
                    void $ hGetLine hOut
                    return "SOMERESULT"
                mbResult `shouldBe` Just (ExitFailure 1, "SOMERESULT")
        it "returns SIGTERM when process terminated" $
            withTestScript $ \scriptPath -> do
                mbResult <- timeoutProcess (fromSeconds 1) $ withProcess shell [scriptPath, "1"] $ \(hIn, hOut, _, ph) -> do
                    hSetBuffering hIn NoBuffering
                    hSetBuffering hOut NoBuffering
                    hPutStrLn hIn "SOMEINPUT"
                    hPutStrLn hIn "DONTQUIT"
                    void $ hGetLine hOut
                    void $ hGetLine hOut
                    terminateProcess ph
                    return "SOMERESULT"
                mbResult `shouldBe` Just (ExitFailure (-15), "SOMERESULT")
        it "should return nothing when it times out" $
            withTestScript $ \scriptPath -> do
                mbResult <- timeoutProcess (fromSeconds 1) $ withProcess shell [scriptPath, "0"] $ \(hIn, hOut, _, _) -> do
                    hSetBuffering hIn NoBuffering
                    hSetBuffering hOut NoBuffering
                    hPutStrLn hIn "SOMEINPUT"
                    hPutStrLn hIn "DONTQUIT"
                    void $ hGetLine hOut
                    void $ hGetLine hOut
                    return "SOMERESULT"
                mbResult `shouldBe` Nothing
    describe "timeoutProcess and withCheckedProcess" $ do
        it "returns when process returns 0" $
            withTestScript $ \scriptPath -> do
                mbResult <- timeoutProcess (fromSeconds 1) $ withCheckedProcess shell [scriptPath, "0"] $ \(hIn, hOut, _, _) -> do
                    hSetBuffering hIn NoBuffering
                    hSetBuffering hOut NoBuffering
                    hPutStrLn hIn "SOMEINPUT"
                    hPutStrLn hIn "QUIT"
                    void $ hGetLine hOut
                    void $ hGetLine hOut
                    return "SOMERESULT"
                mbResult `shouldBe` Just "SOMERESULT"
        it "should throw when process returns non-0" $
            withTestScript $ \scriptPath -> do
                timeoutProcess (fromSeconds 2) $ withCheckedProcess shell [scriptPath, "1"] $ \(hIn, hOut, _, _) -> do
                    hSetBuffering hIn NoBuffering
                    hSetBuffering hOut NoBuffering
                    hPutStrLn hIn "SOMEINPUT"
                    hPutStrLn hIn "QUIT"
                    void $ hGetLine hOut
                    void $ hGetLine hOut
                    return "SOMERESULT"
                `shouldThrow` anyIOException
        it "should return nothing when it times out" $
            withTestScript $ \scriptPath -> do
                mbResult <- timeoutProcess (fromSeconds 1) $ withCheckedProcess shell [scriptPath, "0"] $ \(hIn, hOut, _, _) -> do
                    hSetBuffering hIn NoBuffering
                    hSetBuffering hOut NoBuffering
                    hPutStrLn hIn "SOMEINPUT"
                    hPutStrLn hIn "DONTQUIT"
                    void $ hGetLine hOut
                    void $ hGetLine hOut
                    return "SOMERESULT"
                mbResult `shouldBe` Nothing

withTestScript ::  (FilePath -> IO a) -> IO a
withTestScript f = withSystemTempFile "echo-script" $ \path h -> do
    hPutStrLn h "#!/bin/bash\n\
        \exit_code=$1\n\
        \\n\
        \while read line\n\
        \do\n\
        \  echo \"INPUT: $line\"\n\
        \  if [ \"$line\" == 'QUIT' ]; then\n\
        \    echo 'echo-script exiting'\n\
        \    exit $exit_code\n\
        \  fi\n\
        \done < /dev/stdin\n"
    hClose h
    f path
