{-# LANGUAGE CPP #-}
-- |
-- Module      : System.VMWare.VIX.FFI
-- Copyright   : (c) 2013 Austin Seipp
-- License     : BSD-style
-- Maintainer  : aseipp@pobox.com
-- Stability   : experimental
-- Portability : GHC
--
-- Low-level VMware VIX API bindings.
--
module System.VMware.VIX.FFI
    ( -- * Types
      C_VixHandle
    , C_VixServiceProvider
    , C_VixVMPowerOpOptions

      -- * Constants
      -- ** Service Providers
    , c_VIX_SERVICEPROVIDER_VMWARE_VI_SERVER
    , c_VIX_SERVICEPROVIDER_VMWARE_WORKSTATION
    , c_VIX_SERVICEPROVIDER_VMWARE_WORKSTATION_SHARED
    , c_VIX_SERVICEPROVIDER_VMWARE_PLAYER
    , c_VIX_SERVICEPROVIDER_VMWARE_SERVER
      -- ** Powering on VMs
    , c_VIX_VMPOWEROP_NORMAL
    , c_VIX_VMPOWEROP_LAUNCH_GUI
      -- ** Handles
    , c_VIX_INVALID_HANDLE

      -- * Functions
    , c_vix_connect
    , c_vix_disconnect
    , c_vix_vm_open
    , c_vix_vm_close
    , c_vix_vm_poweron
    , c_vix_vm_poweroff
    ) where
import Data.Int
import Foreign.C.Types
import Foreign.C.String

#include "hs_vix.h"

--
-- Types
--

type C_VixHandle           = #{type VixHandle}
type C_VixServiceProvider  = #{type VixServiceProvider}
type C_VixVMPowerOpOptions = #{type VixVMPowerOpOptions}

--
-- Constants
--

-- | Service provider for ESX/ESXi hosts and VMware Server 2.0.
c_VIX_SERVICEPROVIDER_VMWARE_VI_SERVER :: C_VixServiceProvider
c_VIX_SERVICEPROVIDER_VMWARE_VI_SERVER = #{const VIX_SERVICEPROVIDER_VMWARE_VI_SERVER}

-- | Service provider for VMWare Workstation (locally.)
c_VIX_SERVICEPROVIDER_VMWARE_WORKSTATION :: C_VixServiceProvider
c_VIX_SERVICEPROVIDER_VMWARE_WORKSTATION = #{const VIX_SERVICEPROVIDER_VMWARE_WORKSTATION}

-- | Service provider for VMWare Workstation (shared.)
c_VIX_SERVICEPROVIDER_VMWARE_WORKSTATION_SHARED :: C_VixServiceProvider
c_VIX_SERVICEPROVIDER_VMWARE_WORKSTATION_SHARED = #{const VIX_SERVICEPROVIDER_VMWARE_WORKSTATION_SHARED}

-- | Service provider for VMWare Player.
c_VIX_SERVICEPROVIDER_VMWARE_PLAYER :: C_VixServiceProvider
c_VIX_SERVICEPROVIDER_VMWARE_PLAYER = #{const VIX_SERVICEPROVIDER_VMWARE_PLAYER}

-- | Service provider for VMWare Server 1.0.
c_VIX_SERVICEPROVIDER_VMWARE_SERVER :: C_VixServiceProvider
c_VIX_SERVICEPROVIDER_VMWARE_SERVER = #{const VIX_SERVICEPROVIDER_VMWARE_SERVER}

-- | Constant for launching GUIs when powering on/off.
c_VIX_VMPOWEROP_NORMAL :: C_VixVMPowerOpOptions
c_VIX_VMPOWEROP_NORMAL = #{const VIX_VMPOWEROP_NORMAL}

-- | Constant for launching GUIs when powering on/off.
c_VIX_VMPOWEROP_LAUNCH_GUI :: C_VixVMPowerOpOptions
c_VIX_VMPOWEROP_LAUNCH_GUI = #{const VIX_VMPOWEROP_LAUNCH_GUI}

-- | Constant for an invalid 'C_VixHandle'.
c_VIX_INVALID_HANDLE :: C_VixHandle
c_VIX_INVALID_HANDLE = #{const VIX_INVALID_HANDLE}

--
-- Functions
--

foreign import ccall unsafe "hs_vix_connect"
  c_vix_connect :: CString -> CString -> CString -> CInt ->
                   C_VixServiceProvider -> IO C_VixHandle

foreign import ccall unsafe "hs_vix_disconnect"
  c_vix_disconnect :: C_VixHandle -> IO ()

foreign import ccall unsafe "hs_vix_vm_open"
  c_vix_vm_open :: C_VixHandle -> CString -> IO C_VixHandle

foreign import ccall unsafe "hs_vix_vm_close"
  c_vix_vm_close :: C_VixHandle -> IO ()

foreign import ccall unsafe "hs_vix_vm_poweron"
  c_vix_vm_poweron :: C_VixHandle -> C_VixVMPowerOpOptions -> IO CInt

foreign import ccall unsafe "hs_vix_vm_poweroff"
  c_vix_vm_poweroff :: C_VixHandle -> IO CInt
