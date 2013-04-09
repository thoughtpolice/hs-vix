{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : System.VMWare
-- Copyright   : (c) 2013 Austin Seipp
-- License     : BSD-style
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : GHC
--
-- VMware VIX API bindings.
--
module System.VMware.VIX
    ( -- * Types
      ConnInfo(..)  -- :: *
    , HostHandle    -- :: *
    , VMHandle      -- :: *
      
      -- * Functions
      -- ** Connecting to VMware instances
    , connect       -- :: *
    , disconnect    -- :: *
      
      -- ** Opening virtual machines
    , openVM        -- :: *
    , closeVM       -- :: *
    ) where

import Data.Maybe (fromMaybe)
import Control.Applicative
import Control.Concurrent

import Foreign
import Foreign.C

import System.VMware.VIX.FFI

type Hostname = String
type Port     = Int
type Username = String
type Password = String

-- | The type of system you want to connect to
data ConnInfo
  = VMwareServer2 Hostname (Maybe Port) Username Password
    -- ^ Use this to connect to ESX/ESXi and VMware Server 2.0 systems.
  | VMwareWorkstationRemote Hostname (Maybe Port) Username Password
    -- ^ Use this to connect to local VMware workstation instances.
  | VMwareWorkstationLocal
    -- ^ Use this to connect to local VMware workstation instances.
  | VMwarePlayer
    -- ^ Use this to connect to a local VMware player instance.
  | VMwareServer1 Hostname (Maybe Port) Username Password
    -- ^ Use this to connect to a VMware Server 1.0 machine
  deriving (Eq, Show)

-- | Handle to a remote VMware instance.
data HostHandle = HostHandle (MVar C_VixHandle)

-- | Handle to a virtual machine on a VMware instance.
data VMHandle = VMHandle (MVar C_VixHandle)

-- | Connect to a VMWare system to execute commands.
connect :: ConnInfo -> IO (Either String HostHandle)
connect VMwareWorkstationLocal
  = connectLocal c_VIX_SERVICEPROVIDER_VMWARE_WORKSTATION
connect VMwarePlayer
  = connectLocal c_VIX_SERVICEPROVIDER_VMWARE_PLAYER
connect (VMwareServer2 host port user pass)
  = connectRemote c_VIX_SERVICEPROVIDER_VMWARE_PLAYER host port user pass
connect (VMwareWorkstationRemote host port user pass)
  = connectRemote c_VIX_SERVICEPROVIDER_VMWARE_PLAYER host port user pass
connect (VMwareServer1 host port user pass)
  = connectRemote c_VIX_SERVICEPROVIDER_VMWARE_PLAYER host port user pass

connectLocal :: C_VixServiceProvider -> IO (Either String HostHandle)
connectLocal typ = alloca $ \errOut -> do
  hdl <- c_vix_connect nullPtr nullPtr nullPtr (fromIntegral (0 :: Int)) typ errOut
  if hdl == c_VIX_INVALID_HANDLE then do
    (c_VIX_GET_ERROR_MSG <$> peek errOut) >>= return . Left
   else newMVar hdl >>= return . Right . HostHandle

connectRemote :: C_VixServiceProvider
              -> Hostname -> Maybe Port -> Username -> Password
              -> IO (Either String HostHandle)
connectRemote typ host port user pass =
  let hostname = "https://" ++ host ++ ":" ++ show (fromMaybe 443 port) ++ "/sdk"
      port'    = fromIntegral (0 :: Int) -- The API doesn't use this. wtf
  in withCString hostname $ \chost ->
     withCString user     $ \cuser ->
     withCString pass     $ \cpass ->
     alloca               $ \errOut -> do
       hdl <- c_vix_connect chost cuser cpass port' typ errOut
       if hdl == c_VIX_INVALID_HANDLE then
         (c_VIX_GET_ERROR_MSG <$> peek errOut) >>= return . Left
        else newMVar hdl >>= return . Right . HostHandle

-- | Disconnect from a host.
disconnect :: HostHandle -> IO ()
disconnect (HostHandle hdl) = modifyMVar_ hdl $ \hdl_ ->
  c_vix_disconnect hdl_ >> return c_VIX_INVALID_HANDLE


-- | Open a virtual machine
openVM :: HostHandle -> FilePath -> IO (Either String VMHandle)
openVM (HostHandle hdl) vmxpath = withMVar hdl $ \hdl_ ->
  alloca $ \errOut ->
  withCString vmxpath $ \cvmxpath -> do
    vhdl <- c_vix_vm_open hdl_ cvmxpath errOut
    if vhdl == c_VIX_INVALID_HANDLE then 
      (c_VIX_GET_ERROR_MSG <$> peek errOut) >>= return . Left
     else newMVar vhdl >>= return . Right . VMHandle
      
-- | Close a virtual machine.
closeVM :: VMHandle -> IO ()
closeVM (VMHandle hdl) = modifyMVar_ hdl $ \hdl_ ->
  c_vix_vm_close hdl_ >> return c_VIX_INVALID_HANDLE

--
-- Utilities
--
