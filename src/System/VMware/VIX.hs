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
    ( -- * VIX API
      -- ** Connecting to VMware instances
      Hostname      -- :: *
    , Port          -- :: *
    , Username      -- :: *
    , Password      -- :: *
    , ConnInfo(..)  -- :: *
    , HostHandle    -- :: *
    , connect       -- :: ...
    , disconnect    -- :: ...
      
      -- ** Opening, listing VMs
    , VMHandle      -- :: *
    , openVM        -- :: ...
    , closeVM       -- :: ...

      -- ** Powering on/off and suspending VMs
    , PowerOpts(..)
    , powerOn       -- :: ...
    , powerOff      -- :: ...
    ) where

import Data.Maybe (fromMaybe)
import Control.Applicative
import Control.Concurrent

import Foreign
import Foreign.C

import System.VMware.VIX.FFI

-- | VMware server hostname.
type Hostname = String

-- | VMware server API port. Default is 443.
type Port     = Int

-- | VMware server username. This should be the account
-- to log into remotely (for example, the unix username your
-- VMware workstation software is running on.)
type Username = String

-- | VMware server password.
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

-- | Power options for a VM.
data PowerOpts
  = VMPowerNormal
    -- ^ Power on the virtual machine and continue.
  | VMPowerLaunchGUI
    -- ^ Launch a GUI when powering on the VM.
    --
    -- Only valid with local VMware Workstation and VMware Player
    -- instances.
    --
    -- Does not work with encrypted virtual machines. Will return
    -- a 'not supported' exception.
    deriving (Eq, Show)

-- | Power on a virtual machine.
powerOn :: VMHandle -> PowerOpts -> IO (Either String ())
powerOn (VMHandle hdl) opts = withMVar hdl $ \hdl_ ->
  alloca $ \errOut -> do
    let poweropt | opts == VMPowerLaunchGUI = c_VIX_VMPOWEROP_LAUNCH_GUI
                 | otherwise                = c_VIX_VMPOWEROP_NORMAL
    res <- c_vix_vm_poweron hdl_ poweropt errOut
    wrapE errOut (res == 1) ()

-- | Power off a virtual machine.
powerOff :: VMHandle -> IO (Either String ())
powerOff (VMHandle hdl) = withMVar hdl $ \hdl_ ->
  alloca $ \errOut -> do
    res <- c_vix_vm_poweroff hdl_ errOut
    wrapE errOut (res == 1) ()

--
-- Utilities
--

wrapE :: Ptr C_VixError -> Bool -> r -> IO (Either String r)
wrapE x b r =
  if b then return (Right r)
   else (c_VIX_GET_ERROR_MSG <$> peek x) >>= return . Left
{-# INLINE wrapE #-}
