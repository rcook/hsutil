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
    , StdStream(..)
    , createProcess
    , proc
    , terminateProcess
    , waitForProcess
    )
import System.Timeout (timeout)
import Text.Printf (printf)

import Test.Hspec

type ProcessAction a = (Handle, Handle) -> IO a

newtype Timeout = Timeout { unSeconds :: Int }

fromSeconds :: Int -> Timeout
fromSeconds = Timeout

timeoutProcess :: Timeout -> IO a -> IO (Maybe a)
timeoutProcess t = timeout (unSeconds t * 1000000)

withProcess :: String -> [String] -> ProcessAction a -> IO (ExitCode, a)
withProcess command args f = bracketOnError
    (createProcess (proc command args)
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        })

    (\(_, _, _, pid) -> terminateProcess pid >> waitForProcess pid)

    (\(Just hIn, Just hOut, _, pid) -> do
        result <- f (hIn, hOut)
        exitCode <- waitForProcess pid
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
                mbResult <- timeoutProcess (fromSeconds 1) $ withProcess "sh" [scriptPath, "0"] $ \(hIn, hOut) -> do
                    hSetBuffering hIn NoBuffering
                    hSetBuffering hOut NoBuffering
                    hPutStrLn hIn "SOMEINPUT"
                    hPutStrLn hIn "QUIT"
                    void $ hGetLine hOut
                    void $ hGetLine hOut
                    return ()
                mbResult `shouldBe` Just (ExitSuccess, ())
        it "returns success when process returns 0 and action returns string" $
            withTestScript $ \scriptPath -> do
                mbResult <- timeoutProcess (fromSeconds 1) $ withProcess "sh" [scriptPath, "0"] $ \(hIn, hOut) -> do
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
                mbResult <- timeoutProcess (fromSeconds 1) $ withProcess "sh" [scriptPath, "1"] $ \(hIn, hOut) -> do
                    hSetBuffering hIn NoBuffering
                    hSetBuffering hOut NoBuffering
                    hPutStrLn hIn "SOMEINPUT"
                    hPutStrLn hIn "QUIT"
                    void $ hGetLine hOut
                    void $ hGetLine hOut
                    return "SOMERESULT"
                mbResult `shouldBe` Just (ExitFailure 1, "SOMERESULT")
        it "should return nothing when it times out" $
            withTestScript $ \scriptPath -> do
                mbResult <- timeoutProcess (fromSeconds 1) $ withProcess "sh" [scriptPath, "0"] $ \(hIn, hOut) -> do
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
                mbResult <- timeoutProcess (fromSeconds 1) $ withCheckedProcess "sh" [scriptPath, "0"] $ \(hIn, hOut) -> do
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
                timeoutProcess (fromSeconds 1) $ withCheckedProcess "sh" [scriptPath, "1"] $ \(hIn, hOut) -> do
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
                mbResult <- timeoutProcess (fromSeconds 1) $ withCheckedProcess "sh" [scriptPath, "0"] $ \(hIn, hOut) -> do
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
        \    echo 'echo-demo exiting'\n\
        \    exit $exit_code\n\
        \  fi\n\
        \done < /dev/stdin\n"
    hClose h
    f path
