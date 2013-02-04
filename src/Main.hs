{-# LANGUAGE ScopedTypeVariables,BangPatterns,CPP,TemplateHaskell #-}
import System.Environment (getArgs)
import Control.Monad
import Control.Exception (evaluate)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import MSC

rtable :: RemoteTable
rtable = MSC.__remoteTable initRemoteTable

main :: IO ()
main = do
  args <- getArgs
  
  case args of
    ["MH", host, port] -> do
      backend <- initializeBackend host port rtable
      startMaster backend $ \nodes -> do
        -- spawn all MSCs
        mscPs <- mapM (\(there) -> spawn there ($(mkClosure 'mscP) (0::Int))) nodes
        mhP sampleMH mscPs
    ["MSC", host, port] -> do
      backend <- initializeBackend host port rtable
      startSlave backend
