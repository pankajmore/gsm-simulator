{-# LANGUAGE ScopedTypeVariables,BangPatterns,CPP,TemplateHaskell #-} 
module MSC where

import Control.Monad
import Control.Distributed.Process
import Types
import Control.Distributed.Process.Closure
import qualified Data.List as L

sampleMH :: MH
sampleMH = MH { imei = 123456789
            , mh_msisdn = MSISDN { cc = 91
                              , ndc = 87656
                              , sn = 96144
                              }
            , sim = SIM { iccid = ICCID { iin = 43
                                        , iai = 635
                                        , chk = 1
                                        }
                        , s_imsi = IMSI { mcc = 321
                                      , mnc = 45
                                      , msin = 5267
                                      }
                        , ki = 21
                        , lai = 337
                        }
            }                     

attachISMI :: Cell -> MH -> MSC -> MSC
attachISMI c mh msc = msc { hlr =  (hlr msc) ++ [e] }
                     where e = HLREntry { h_imsi = s_imsi $ sim $ mh
                                        , h_msisdn = mh_msisdn mh
                                        , vlrAddr = Nothing
                                        , h_cellNumber = c
                                        }

addtoVLR addr cell hlr msc = msc { vlr = (vlr msc) ++ [e] }
                    where e = VLREntry { v_imsi = h_imsi hlr
                                       , v_msisdn = h_msisdn hlr
                                       , hlrAddr = addr
                                       , v_cellNumber = cell
                                       }

getHLREntryByMSISDN :: MSISDN -> MSC -> Maybe HLREntry 
getHLREntryByMSISDN m msc = L.find (\x -> h_msisdn x == m) (hlr msc)

updatevlrAddrByHLREntry :: MSCAddr -> HLREntry -> MSC -> MSC
updatevlrAddrByHLREntry addr hlrEntry msc = let h = hlr msc in
                                    msc { hlr = (L.delete hlrEntry h) ++ [e] }
                                   where e = hlrEntry { vlrAddr = Just addr }  
                                          
updatevlrAddrByMSISDN :: MSCAddr -> MSISDN -> MSC -> Process MSC 
updatevlrAddrByMSISDN addr m msc = case getHLREntryByMSISDN m msc of
                      Just h -> return $ updatevlrAddrByHLREntry addr h msc
                      _ -> return msc

checkMSISDN :: MSISDN -> HLREntry -> Bool
checkMSISDN m hlrEntry = h_msisdn hlrEntry == m


mscP :: Int -> Process ()
mscP i = forever $ do
  self <- getSelfPid
  let msc = MSC { addr = self
                , hlr = []
                , vlr = []
                }     
  go msc 
 where
  go msc = do
   self <- getSelfPid
   receiveWait
    [ match $ \("Attach IMSI",mhPid::ProcessId,mh::MH,cell::Cell) -> do 
                                      let msc = attachISMI cell mh msc 
                                      send mhPid msc
                                      go msc
              
    , match $ \("Home at",homeMSC,msisdn::MSISDN,cell::Cell)-> do
       self <- getSelfPid
       send (homeMSC) ("Query",msisdn,self) 

       send homeMSC ("UpdateVLRAddr",msisdn,self)
       go msc
    , match $ \("UpdateVLRAddr",msisdn,self) -> do
            newmsc <- updatevlrAddrByMSISDN self msisdn msc
            liftIO $ print newmsc 
            go newmsc
    , match $ \("Query",msisdn,vMSC) -> do 
                                    let ans = L.find (checkMSISDN msisdn) (hlr msc)     
                                    self <- getSelfPid
                                    send vMSC (ans,self)
                                    go msc

    , match $ \("Status",pid) -> do
            send pid msc
            liftIO $ print msc
            go msc

    , match $ \() -> go msc
    ]

remotable ['mscP]

-- Get status of MSC
getMSCStatus :: ProcessId -> ProcessId -> Process()
getMSCStatus mhPid mscPid = do
             send mscPid ("Status",mhPid)
             e ::MSC <- expect
             liftIO $ print "Status"

-- IMSI Attach Command
attachIMSI mhPid mh cellLocation mscPid = do
  send mscPid ("Attach ISMI",mhPid,mh,cellLocation)
  e <- expect 
  liftIO $ putStrLn $ "Attached IMSI" ++ show (e :: MSC) 


mhP :: MH -> [ProcessId] -> Process()
mhP mh mscPs = do
  us <- getSelfPid

  -- choose 1st MSC as the Home MSC
  let homeMSC = head mscPs
  -- Status of homeMSC
  getMSCStatus us homeMSC
  
  --monitor homeMSC 
  --send homeMSC (mh,123::Cell)
  --e <- expect 
  --liftIO $ print $ show (e :: ProcessMonitorNotification) 
  -- Associate with Home MSC at cell number 123
  
  send homeMSC ("Status",us)
  --e :: MSC <- expect 
  --liftIO $ print "Status"
  let vMSC = mscPs!!1
  let msisdn = mh_msisdn mh
  liftIO $ putStrLn $ "Moving to visitor MSC " ++ (show vMSC)  ++ " at cell number 333" 
  send vMSC ("Home at"::String,homeMSC,msisdn::MSISDN,333::Int)
  send vMSC ("Status",us)
  
