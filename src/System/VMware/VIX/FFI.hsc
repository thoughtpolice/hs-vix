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
      -- ** Errors
    , c_VIX_ERROR_CODES
    , c_VIX_GET_ERROR_CODE
    , c_VIX_GET_ERROR_MSG

      -- * Functions
    , c_vix_connect
    , c_vix_disconnect
    , c_vix_vm_open
    , c_vix_vm_close
    , c_vix_vm_poweron
    , c_vix_vm_poweroff
    ) where

import Data.Int
import Data.Bits ((.&.))
import Data.Maybe (fromMaybe)
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


-- | Lookup a VIX error code and return the error message for it.
c_VIX_GET_ERROR_MSG :: CInt -> String
c_VIX_GET_ERROR_MSG x
  = fromMaybe (error "Could not find VIX error code!")
  $ lookup (c_VIX_GET_ERROR_CODE x) c_VIX_ERROR_CODES

-- | Given a returned 'CInt' from a function, representing the VIX
-- error code, mask out the required bits to match the right case
-- inside 'c_VIX_ERROR_CODES'.
c_VIX_GET_ERROR_CODE :: CInt -> CInt
c_VIX_GET_ERROR_CODE x = x .&. 0xFFFF
{-# INLINE c_VIX_GET_ERROR_CODE #-}


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


--
-- Massive error-code table
--

-- | Global error code table.
c_VIX_ERROR_CODES :: [(CInt, String)]
c_VIX_ERROR_CODES =
  [ -- Generated by ./util/gen-error-tbl.pl
    (#{const VIX_OK}, "The operation was successful.")
  , (#{const VIX_E_FAIL}, "Unknown error.")
  , (#{const VIX_E_OUT_OF_MEMORY}, "Memory allocation failed. Out of memory.")
  , (#{const VIX_E_INVALID_ARG}, "One of the parameters was invalid.")
  , (#{const VIX_E_FILE_NOT_FOUND}, "A file was not found.")
  , (#{const VIX_E_OBJECT_IS_BUSY}, "This function cannot be performed because the handle is executing another function.")
  , (#{const VIX_E_NOT_SUPPORTED}, "The operation is not supported.")
  , (#{const VIX_E_FILE_ERROR}, "A file access error occurred on the host or guest operating system.")
  , (#{const VIX_E_DISK_FULL}, "An error occurred while writing a file; the disk is full. Data has not been saved. Free some disk space and try again.")
  , (#{const VIX_E_INCORRECT_FILE_TYPE}, "An error occurred while accessing a file: wrong file type.")
  , (#{const VIX_E_CANCELLED}, "The operation was canceled.")
  , (#{const VIX_E_FILE_READ_ONLY}, "The file is write-protected.")
  , (#{const VIX_E_FILE_ALREADY_EXISTS}, "The file already exists.")
  , (#{const VIX_E_FILE_ACCESS_ERROR}, "You do not have access rights to this file.")
  , (#{const VIX_E_REQUIRES_LARGE_FILES}, "The file system does not support large files.")
  , (#{const VIX_E_FILE_ALREADY_LOCKED}, "The file is already in use.")
  , (#{const VIX_E_VMDB}, "The system returned an error. Communication with the virtual machine might have been interrupted.")
  , (#{const VIX_E_NOT_SUPPORTED_ON_REMOTE_OBJECT}, "The command is not supported on remote objects.")
  , (#{const VIX_E_FILE_TOO_BIG}, "The file is too big for the filesystem.")
  , (#{const VIX_E_FILE_NAME_INVALID}, "The file name is not valid.")
  , (#{const VIX_E_ALREADY_EXISTS}, "Already exists.")
  , (#{const VIX_E_BUFFER_TOOSMALL}, "Buffer is too small.")
  , (#{const VIX_E_OBJECT_NOT_FOUND}, "The request refers to an object that does not exist.")
  , (#{const VIX_E_HOST_NOT_CONNECTED}, "Unable to connect to the host.")
  , (#{const VIX_E_INVALID_UTF8_STRING}, "The string parameter has incorrect encoding.")
  , (#{const VIX_E_OPERATION_ALREADY_IN_PROGRESS}, "The operation is already in progress.")
  , (#{const VIX_E_UNFINISHED_JOB}, "The job has not finished.")
  , (#{const VIX_E_NEED_KEY}, "A decryption key is required to perform the operation.")
  , (#{const VIX_E_LICENSE}, "This operation is not supported with the current license.")
  , (#{const VIX_E_VM_HOST_DISCONNECTED}, "Unable to communicate with the virtual machine's host because it is disconnected.")
  , (#{const VIX_E_AUTHENTICATION_FAIL}, "Authentication for Encrypted VM failed.")
  , (#{const VIX_E_HOST_CONNECTION_LOST}, "The connection to the host was lost.")
  , (#{const VIX_E_DUPLICATE_NAME}, "Another object is using this name.")
  , (#{const VIX_E_INVALID_HANDLE}, "The handle is not a valid VIX object.")
  , (#{const VIX_E_NOT_SUPPORTED_ON_HANDLE_TYPE}, "The operation is not supported on this type of handle.")
  , (#{const VIX_E_TOO_MANY_HANDLES}, "Too many handles are open.")
  , (#{const VIX_E_NOT_FOUND}, "Invalid file. A required section of the file is missing.")
  , (#{const VIX_E_TYPE_MISMATCH}, "Invalid file. An object has the wrong type.")
  , (#{const VIX_E_INVALID_XML}, "Invalid file. The contents might be corrupt.")
  , (#{const VIX_E_TIMEOUT_WAITING_FOR_TOOLS}, "A timeout error occurred while waiting for .")
  , (#{const VIX_E_UNRECOGNIZED_COMMAND}, "The command is not recognized by the virtual machine.")
  , (#{const VIX_E_OP_NOT_SUPPORTED_ON_GUEST}, "The requested operation is not supported on this guest operating system.")
  , (#{const VIX_E_PROGRAM_NOT_STARTED}, "A program could not run on the guest operating system.")
  , (#{const VIX_E_CANNOT_START_READ_ONLY_VM}, "Cannot power on a read-only virtual machine.")
  , (#{const VIX_E_VM_NOT_RUNNING}, "The virtual machine needs to be powered on.")
  , (#{const VIX_E_VM_IS_RUNNING}, "The virtual machine should not be powered on. It is already running.")
  , (#{const VIX_E_CANNOT_CONNECT_TO_VM}, "Cannot connect to the virtual machine.")
  , (#{const VIX_E_POWEROP_SCRIPTS_NOT_AVAILABLE}, "Cannot execute scripts.")
  , (#{const VIX_E_NO_GUEST_OS_INSTALLED}, "There is no operating system installed in the virtual machine.")
  , (#{const VIX_E_VM_INSUFFICIENT_HOST_MEMORY}, "Not enough physical memory is available to power on this virtual machine.")
  , (#{const VIX_E_SUSPEND_ERROR}, "An error occurred while suspending the virtual machine.")
  , (#{const VIX_E_VM_NOT_ENOUGH_CPUS}, "This virtual machine is configured to run with 2 CPUs, but the host has only 1 CPU. The virtual machine cannot be powered on.")
  , (#{const VIX_E_HOST_USER_PERMISSIONS}, "Insufficient permissions in the host operating system.")
  , (#{const VIX_E_GUEST_USER_PERMISSIONS}, "Authentication failure or insufficient permissions in guest operating system.")
  , (#{const VIX_E_TOOLS_NOT_RUNNING}, "are not running in the guest.")
  , (#{const VIX_E_GUEST_OPERATIONS_PROHIBITED}, "Guest operations are not allowed on this virtual machine.")
  , (#{const VIX_E_ANON_GUEST_OPERATIONS_PROHIBITED}, "Anonymous guest operations are not allowed on this virtual machine. You must call VixVM_LoginInGuest before performing guest operations.")
  , (#{const VIX_E_ROOT_GUEST_OPERATIONS_PROHIBITED}, "Guest operations are not allowed for the administrative user on this virtual machine.")
  , (#{const VIX_E_MISSING_ANON_GUEST_ACCOUNT}, "The virtual machine configuration must specify the guest account name to be used for anonymous guest operations.")
  , (#{const VIX_E_CANNOT_AUTHENTICATE_WITH_GUEST}, "The virtual machine cannot authenticate users with guest.")
  , (#{const VIX_E_UNRECOGNIZED_COMMAND_IN_GUEST}, "The command is not recognized by .")
  , (#{const VIX_E_CONSOLE_GUEST_OPERATIONS_PROHIBITED}, "Guest operations are not allowed for console users on this virtual machine.")
  , (#{const VIX_E_MUST_BE_CONSOLE_USER}, "Only the console user can run the command.")
  , (#{const VIX_E_VMX_MSG_DIALOG_AND_NO_UI}, "The virtual machine is blocked waiting for a user operation.")
--, (#{const VIX_E_NOT_ALLOWED_DURING_VM_RECORDING}, "Not allowed while the virtual machine is recording.")
--, (#{const VIX_E_NOT_ALLOWED_DURING_VM_REPLAY}, "Not allowed while the virtual machine is replaying.")
  , (#{const VIX_E_OPERATION_NOT_ALLOWED_FOR_LOGIN_TYPE}, "The command is not allowed by this login type.")
  , (#{const VIX_E_LOGIN_TYPE_NOT_SUPPORTED}, "This login type is not supported.")
  , (#{const VIX_E_EMPTY_PASSWORD_NOT_ALLOWED_IN_GUEST}, "The guest OS does not support empty passwords.")
  , (#{const VIX_E_INTERACTIVE_SESSION_NOT_PRESENT}, "The specified guest user must be logged in interactively to perform this operation.")
  , (#{const VIX_E_INTERACTIVE_SESSION_USER_MISMATCH}, "The specified guest user does not match the user currently logged in interactively.")
--, (#{const VIX_E_UNABLE_TO_REPLAY_VM}, "Unable to replay the virtual machine.")
  , (#{const VIX_E_CANNOT_POWER_ON_VM}, "The virtual machine could not start.")
  , (#{const VIX_E_NO_DISPLAY_SERVER}, "Cannot launch the UI because no display server is present in the current environment.")
--, (#{const VIX_E_VM_NOT_RECORDING}, "The operation failed because the virtual machine is not recording.")
--, (#{const VIX_E_VM_NOT_REPLAYING}, "The operation failed because the virtual machine is not replaying.")
  , (#{const VIX_E_TOO_MANY_LOGONS}, "The supported number of active authentication sessions has been exceeded.")
  , (#{const VIX_E_INVALID_AUTHENTICATION_SESSION}, "The authenticaton session provided does not exist.")
  , (#{const VIX_E_VM_NOT_FOUND}, "The virtual machine cannot be found.")
  , (#{const VIX_E_NOT_SUPPORTED_FOR_VM_VERSION}, "The operation is not supported for this virtual machine version.")
  , (#{const VIX_E_CANNOT_READ_VM_CONFIG}, "Cannot read the virtual machine configuration file.")
  , (#{const VIX_E_TEMPLATE_VM}, "Cannot perform this operation on a template virtual machine.")
  , (#{const VIX_E_VM_ALREADY_LOADED}, "The virtual machine has already been loaded.")
  , (#{const VIX_E_VM_ALREADY_UP_TO_DATE}, "The virtual machine is already up-to-date.")
  , (#{const VIX_E_VM_UNSUPPORTED_GUEST}, "The specified guest operating system is not supported on the host that is the target of the operation.")
  , (#{const VIX_E_UNRECOGNIZED_PROPERTY}, "Unrecognized handle property identifier.")
  , (#{const VIX_E_INVALID_PROPERTY_VALUE}, "Invalid property value.")
  , (#{const VIX_E_READ_ONLY_PROPERTY}, "Cannot change a read-only property.")
  , (#{const VIX_E_MISSING_REQUIRED_PROPERTY}, "This handle is missing a required property.")
  , (#{const VIX_E_INVALID_SERIALIZED_DATA}, "A serialized object is invalid and cannot be deserialized.")
  , (#{const VIX_E_PROPERTY_TYPE_MISMATCH}, "The data provided does not match the property type.")
  , (#{const VIX_E_BAD_VM_INDEX}, "The index parameter does not correspond to a result set.")
  , (#{const VIX_E_INVALID_MESSAGE_HEADER}, "A message header was corrupted or has the incorrect version.")
  , (#{const VIX_E_INVALID_MESSAGE_BODY}, "A message body was corrupted or is missing.")
  , (#{const VIX_E_SNAPSHOT_INVAL}, "A snapshot-related error has occurred.")
  , (#{const VIX_E_SNAPSHOT_DUMPER}, "Unable to open the snapshot file.")
  , (#{const VIX_E_SNAPSHOT_DISKLIB}, "Disk error.")
  , (#{const VIX_E_SNAPSHOT_NOTFOUND}, "The snapshot does not exist.")
  , (#{const VIX_E_SNAPSHOT_EXISTS}, "The snapshot already exists.")
  , (#{const VIX_E_SNAPSHOT_VERSION}, "Snapshots are not allowed on this virtual machine.")
  , (#{const VIX_E_SNAPSHOT_NOPERM}, "Insufficient permissions.")
  , (#{const VIX_E_SNAPSHOT_CONFIG}, "There is an error in the configuration file.")
  , (#{const VIX_E_SNAPSHOT_NOCHANGE}, "The state of the virtual machine has not changed since the last snapshot operation.")
  , (#{const VIX_E_SNAPSHOT_CHECKPOINT}, "Unable to save the snapshot file.")
  , (#{const VIX_E_SNAPSHOT_LOCKED}, "A snapshot operation is already in progress.")
  , (#{const VIX_E_SNAPSHOT_INCONSISTENT}, "The snapshot files are in an inconsistent state.")
  , (#{const VIX_E_SNAPSHOT_NAMETOOLONG}, "The filename is too long.")
  , (#{const VIX_E_SNAPSHOT_VIXFILE}, "Cannot snapshot all metadata files.")
  , (#{const VIX_E_SNAPSHOT_DISKLOCKED}, "One or more of the disks are busy.")
  , (#{const VIX_E_SNAPSHOT_DUPLICATEDDISK}, "The virtual disk is used multiple times.")
  , (#{const VIX_E_SNAPSHOT_INDEPENDENTDISK}, "Cannot take snapshots of powered on virtual machines with independent disks.")
  , (#{const VIX_E_SNAPSHOT_NONUNIQUE_NAME}, "The name does not uniquely identify one snapshot.")
  , (#{const VIX_E_SNAPSHOT_MEMORY_ON_INDEPENDENT_DISK}, "Failed to take a memory snapshot because the virtual machine is configured with independent disks.")
  , (#{const VIX_E_SNAPSHOT_MAXSNAPSHOTS}, "Exceeded the maximum number of permitted snapshots.")
  , (#{const VIX_E_SNAPSHOT_MIN_FREE_SPACE}, "Available free space is less than the configured minimum free space.")
  , (#{const VIX_E_SNAPSHOT_HIERARCHY_TOODEEP}, "Snapshot hierarchy is too deep.")
  , (#{const VIX_E_SNAPSHOT_RRSUSPEND}, "Snapshots are not allowed on .")
  , (#{const VIX_E_SNAPSHOT_NOT_REVERTABLE}, "Cannot revert. The snapshot is .")
  , (#{const VIX_E_HOST_DISK_INVALID_VALUE}, "The specified device is not a valid physical disk device.")
  , (#{const VIX_E_HOST_DISK_SECTORSIZE}, "The disk sector size check failed.")
  , (#{const VIX_E_HOST_FILE_ERROR_EOF}, "Read beyond the end of file.")
  , (#{const VIX_E_HOST_NETBLKDEV_HANDSHAKE}, "Error in protocol.")
  , (#{const VIX_E_HOST_SOCKET_CREATION_ERROR}, "Unable to create a socket.")
  , (#{const VIX_E_HOST_SERVER_NOT_FOUND}, "The specified server could not be contacted.")
  , (#{const VIX_E_HOST_NETWORK_CONN_REFUSED}, "The server refused connection.")
  , (#{const VIX_E_HOST_TCP_SOCKET_ERROR}, "There was an error in communication.")
  , (#{const VIX_E_HOST_TCP_CONN_LOST}, "The connection was lost.")
  , (#{const VIX_E_HOST_NBD_HASHFILE_VOLUME}, "NBD_ERR_HASHFILE_VOLUME.")
  , (#{const VIX_E_HOST_NBD_HASHFILE_INIT}, "NBD_ERR_HASHFILE_INIT.")
  , (#{const VIX_E_DISK_INVAL}, "One of the parameters supplied is invalid.")
  , (#{const VIX_E_DISK_NOINIT}, "The disk library has not been initialized.")
  , (#{const VIX_E_DISK_NOIO}, "The called function requires the virtual disk to be opened for I/O.")
  , (#{const VIX_E_DISK_PARTIALCHAIN}, "The called function cannot be performed on partial chains. Open the parent virtual disk.")
  , (#{const VIX_E_DISK_NEEDSREPAIR}, "The specified virtual disk needs repair.")
  , (#{const VIX_E_DISK_OUTOFRANGE}, "You have requested access to an area of the virtual disk that is out of bounds.")
  , (#{const VIX_E_DISK_CID_MISMATCH}, "The parent virtual disk has been modified since the child was created. Parent virutal disk's content ID does not match with the parent content ID in the child.")
  , (#{const VIX_E_DISK_CANTSHRINK}, "The specified virtual disk cannot be shrunk because it is not the parent disk.")
  , (#{const VIX_E_DISK_PARTMISMATCH}, "The partition table on the physical disk has changed since the disk was created. Remove the physical disk from the virtual machine, then add it again.")
  , (#{const VIX_E_DISK_UNSUPPORTEDDISKVERSION}, "than the version supported by this program.")
  , (#{const VIX_E_DISK_OPENPARENT}, "The parent of this virtual disk could not be opened.")
  , (#{const VIX_E_DISK_NOTSUPPORTED}, "The specified feature is not supported by this version.")
  , (#{const VIX_E_DISK_NEEDKEY}, "One or more required keys were not provided.")
  , (#{const VIX_E_DISK_NOKEYOVERRIDE}, "Will not create an unencrypted child of an encrypted disk without explicit request.")
  , (#{const VIX_E_DISK_NOTENCRYPTED}, "Not an encrypted disk.")
  , (#{const VIX_E_DISK_NOKEY}, "No keys were supplied for encrypting the disk.")
  , (#{const VIX_E_DISK_INVALIDPARTITIONTABLE}, "The partition table is invalid.")
  , (#{const VIX_E_DISK_NOTNORMAL}, "Only sparse extents with embedded descriptors can be encrypted.")
  , (#{const VIX_E_DISK_NOTENCDESC}, "Not an encrypted descriptor file.")
  , (#{const VIX_E_DISK_NEEDVMFS}, "The file system is not VMFS.")
  , (#{const VIX_E_DISK_RAWTOOBIG}, "The physical disk is too big. The maximum size allowed is 2TB.")
  , (#{const VIX_E_DISK_TOOMANYOPENFILES}, "The host's limit for open files has been exceeded.")
  , (#{const VIX_E_DISK_TOOMANYREDO}, "Too many levels of redo logs.")
  , (#{const VIX_E_DISK_RAWTOOSMALL}, "The physical disk is too small.")
  , (#{const VIX_E_DISK_INVALIDCHAIN}, "Invalid disk chain: cannot mix hosted and managed style disks in the same chain.")
  , (#{const VIX_E_DISK_KEY_NOTFOUND}, "The specified key is not found in the disk database.")
  , (#{const VIX_E_DISK_SUBSYSTEM_INIT_FAIL}, "One or more required subsystems failed to initialize.")
  , (#{const VIX_E_DISK_INVALID_CONNECTION}, "Invalid connection handle.")
  , (#{const VIX_E_DISK_ENCODING}, "Disk encoding error.")
  , (#{const VIX_E_DISK_CANTREPAIR}, "The disk is corrupted and unrepairable.")
  , (#{const VIX_E_DISK_INVALIDDISK}, "The specified file is not a virtual disk.")
  , (#{const VIX_E_DISK_NOLICENSE}, "The host is not licensed for this feature.")
  , (#{const VIX_E_DISK_NODEVICE}, "The device does not exist.")
  , (#{const VIX_E_DISK_UNSUPPORTEDDEVICE}, "The operation is not supported on this type of device.")
  , (#{const VIX_E_DISK_CAPACITY_MISMATCH}, "The parent virtual disk's capacity is not the same as child's capacity.")
  , (#{const VIX_E_DISK_PARENT_NOTALLOWED}, "Disk type cannot be allowed as parent.")
  , (#{const VIX_E_DISK_ATTACH_ROOTLINK}, "Both parent and child virtual disks are root links.")
  , (#{const VIX_E_CRYPTO_UNKNOWN_ALGORITHM}, "Security library error.")
  , (#{const VIX_E_CRYPTO_BAD_BUFFER_SIZE}, "Security library error.")
  , (#{const VIX_E_CRYPTO_INVALID_OPERATION}, "Security library error.")
  , (#{const VIX_E_CRYPTO_RANDOM_DEVICE}, "Security library error.")
  , (#{const VIX_E_CRYPTO_NEED_PASSWORD}, "A password is required for this operation.")
  , (#{const VIX_E_CRYPTO_BAD_PASSWORD}, "Incorrect password.")
  , (#{const VIX_E_CRYPTO_NOT_IN_DICTIONARY}, "Security library error.")
  , (#{const VIX_E_CRYPTO_NO_CRYPTO}, "Security library error.")
  , (#{const VIX_E_CRYPTO_ERROR}, "Security library error.")
  , (#{const VIX_E_CRYPTO_BAD_FORMAT}, "Security library error.")
  , (#{const VIX_E_CRYPTO_LOCKED}, "Security library error.")
  , (#{const VIX_E_CRYPTO_EMPTY}, "Security library error.")
  , (#{const VIX_E_CRYPTO_KEYSAFE_LOCATOR}, "Security library error.")
  , (#{const VIX_E_CANNOT_CONNECT_TO_HOST}, "Cannot connect to the host.")
  , (#{const VIX_E_NOT_FOR_REMOTE_HOST}, "Only a local host can support this feature.")
  , (#{const VIX_E_INVALID_HOSTNAME_SPECIFICATION}, "Malformed hostname parameter. For the given service provider, the hostname must be a URL in the form https://:/sdk.")
  , (#{const VIX_E_SCREEN_CAPTURE_ERROR}, "Could not capture screen.")
  , (#{const VIX_E_SCREEN_CAPTURE_BAD_FORMAT}, "Requested unsupported format.")
  , (#{const VIX_E_SCREEN_CAPTURE_COMPRESSION_FAIL}, "Could not compress the screen capture.")
  , (#{const VIX_E_SCREEN_CAPTURE_LARGE_DATA}, "The screen capture data is larger than the maximum size.")
  , (#{const VIX_E_GUEST_VOLUMES_NOT_FROZEN}, "The drives are not frozen.")
  , (#{const VIX_E_NOT_A_FILE}, "The object is not a file.")
  , (#{const VIX_E_NOT_A_DIRECTORY}, "The object is not a directory.")
  , (#{const VIX_E_NO_SUCH_PROCESS}, "No such process.")
  , (#{const VIX_E_FILE_NAME_TOO_LONG}, "File name too long.")
  , (#{const VIX_E_OPERATION_DISABLED}, "The operation has been disabled by the guest operating system.")
  , (#{const VIX_E_TOOLS_INSTALL_NO_IMAGE}, "No .")
  , (#{const VIX_E_TOOLS_INSTALL_IMAGE_INACCESIBLE}, "The .")
  , (#{const VIX_E_TOOLS_INSTALL_NO_DEVICE}, "The guest operating system does not have a device configured for the .")
  , (#{const VIX_E_TOOLS_INSTALL_DEVICE_NOT_CONNECTED}, "The guest operating system device used for installation of .")
  , (#{const VIX_E_TOOLS_INSTALL_CANCELLED}, "The .")
  , (#{const VIX_E_TOOLS_INSTALL_INIT_FAILED}, "The .")
  , (#{const VIX_E_TOOLS_INSTALL_AUTO_NOT_SUPPORTED}, "The .")
  , (#{const VIX_E_TOOLS_INSTALL_GUEST_NOT_READY}, "are not running in the guest OS. Automatic upgrade is not possible.")
  , (#{const VIX_E_TOOLS_INSTALL_SIG_CHECK_FAILED}, "The .")
  , (#{const VIX_E_TOOLS_INSTALL_ERROR}, "The .")
  , (#{const VIX_E_TOOLS_INSTALL_ALREADY_UP_TO_DATE}, "are already up to date.")
  , (#{const VIX_E_TOOLS_INSTALL_IN_PROGRESS}, "A .")
  , (#{const VIX_E_TOOLS_INSTALL_IMAGE_COPY_FAILED}, "Could not copy .")
  , (#{const VIX_E_WRAPPER_WORKSTATION_NOT_INSTALLED}, "Service type VIX_SERVICEPROVIDER_VMWARE_WORKSTATION was specified but not installed.")
  , (#{const VIX_E_WRAPPER_VERSION_NOT_FOUND}, "The specified version was not found.")
  , (#{const VIX_E_WRAPPER_SERVICEPROVIDER_NOT_FOUND}, "The specified service provider was not found.")
  , (#{const VIX_E_WRAPPER_PLAYER_NOT_INSTALLED}, "Service type VIX_SERVICEPROVIDER_VMWARE_PLAYER was specified but not installed.")
  , (#{const VIX_E_WRAPPER_RUNTIME_NOT_INSTALLED}, "Cannot find support libraries; VIX appears to have not been installed.")
  , (#{const VIX_E_WRAPPER_MULTIPLE_SERVICEPROVIDERS}, "Cannot connect with multiple service providers.")
  , (#{const VIX_E_MNTAPI_MOUNTPT_NOT_FOUND}, "Could not find the specified mountpoint.")
  , (#{const VIX_E_MNTAPI_MOUNTPT_IN_USE}, "The mountpoint is already in use.")
  , (#{const VIX_E_MNTAPI_DISK_NOT_FOUND}, "Could not find the specified virtual disk.")
  , (#{const VIX_E_MNTAPI_DISK_NOT_MOUNTED}, "The specified disk is not mounted.")
  , (#{const VIX_E_MNTAPI_DISK_IS_MOUNTED}, "The specified disk is already mounted.")
  , (#{const VIX_E_MNTAPI_DISK_NOT_SAFE}, "It is not safe to mount the virtual disk. It might be attached to a suspended or powered-on VM, or it may be inside a snapshot chain.")
  , (#{const VIX_E_MNTAPI_DISK_CANT_OPEN}, "Cannot open the virtual disk.")
  , (#{const VIX_E_MNTAPI_CANT_READ_PARTS}, "Cannot read or parse the partition table on the virtual disk.")
  , (#{const VIX_E_MNTAPI_UMOUNT_APP_NOT_FOUND}, "Could not find the umount application in a standard system directory such as /bin, /usr/bin, or /sbin.")
  , (#{const VIX_E_MNTAPI_UMOUNT}, "The umount command failed.")
  , (#{const VIX_E_MNTAPI_NO_MOUNTABLE_PARTITONS}, "The virtual disk does not have any partitions that the host system knows how to mount.")
  , (#{const VIX_E_MNTAPI_PARTITION_RANGE}, "An invalid partition number was specified.")
  , (#{const VIX_E_MNTAPI_PERM}, "Insufficient permissions to perform this operation.")
  , (#{const VIX_E_MNTAPI_DICT}, "Error accessing metadata. You might not have sufficient permission to access this disk or the metadata may be corrupted.")
  , (#{const VIX_E_MNTAPI_DICT_LOCKED}, "The metadata for this disk is locked. Check for other running virtual disk mounter applications.")
  , (#{const VIX_E_MNTAPI_OPEN_HANDLES}, "Another process is performing an operation on this mounted virtual disk.")
  , (#{const VIX_E_MNTAPI_CANT_MAKE_VAR_DIR}, "Cannot create directory '/var/run/vmware/fuse'.")
  , (#{const VIX_E_MNTAPI_NO_ROOT}, "This application must be run setuid root.")
  , (#{const VIX_E_MNTAPI_LOOP_FAILED}, "A loop device operation failed.")
  , (#{const VIX_E_MNTAPI_DAEMON}, "The VMware fuse daemon failed to start.")
  , (#{const VIX_E_MNTAPI_INTERNAL}, "An internal error has occurred. Contact VMware support.")
  , (#{const VIX_E_MNTAPI_SYSTEM}, "A system call has failed.")
  , (#{const VIX_E_MNTAPI_NO_CONNECTION_DETAILS}, "Unable to get vixDiskLib connection details.")
  , (#{const VIX_E_MNTAPI_INCOMPATIBLE_VERSION}, "The product version number is lower than the expected version number.")
  , (#{const VIX_E_MNTAPI_OS_ERROR}, "There was an operating system error.")
  , (#{const VIX_E_MNTAPI_DRIVE_LETTER_IN_USE}, "The specified drive letter is already in use.")
  , (#{const VIX_E_MNTAPI_DRIVE_LETTER_ALREADY_ASSIGNED}, "The specified drive letter is already assigned.")
  , (#{const VIX_E_MNTAPI_VOLUME_NOT_MOUNTED}, "The specified volume is not mounted.")
  , (#{const VIX_E_MNTAPI_VOLUME_ALREADY_MOUNTED}, "The specified volume is already mounted.")
  , (#{const VIX_E_MNTAPI_FORMAT_FAILURE}, "Unable to format volume.")
  , (#{const VIX_E_MNTAPI_NO_DRIVER}, "Driver not found.")
  , (#{const VIX_E_MNTAPI_ALREADY_OPENED}, "A handle to the Volume or DiskSet is already open.")
  , (#{const VIX_E_MNTAPI_ITEM_NOT_FOUND}, "Invalid file. A required section of the file is missing.")
  , (#{const VIX_E_MNTAPI_UNSUPPROTED_BOOT_LOADER}, "Boot loader not supported.")
  , (#{const VIX_E_MNTAPI_UNSUPPROTED_OS}, "The current operating system is not supported.")
  , (#{const VIX_E_MNTAPI_CODECONVERSION}, "An error occurred while converting the string.")
  , (#{const VIX_E_MNTAPI_REGWRITE_ERROR}, "There was an error writing to the registry.")
  , (#{const VIX_E_MNTAPI_UNSUPPORTED_FT_VOLUME}, "Windows NT4 Fault Tolerant volume type is not supported.")
  , (#{const VIX_E_MNTAPI_PARTITION_NOT_FOUND}, "The specified partition was not found.")
  , (#{const VIX_E_MNTAPI_PUTFILE_ERROR}, "Putfile error.")
  , (#{const VIX_E_MNTAPI_GETFILE_ERROR}, "Getfile error.")
  , (#{const VIX_E_MNTAPI_REG_NOT_OPENED}, "Unable to open registry key.")
  , (#{const VIX_E_MNTAPI_REGDELKEY_ERROR}, "There was an error deleting the registry key.")
  , (#{const VIX_E_MNTAPI_CREATE_PARTITIONTABLE_ERROR}, "An error occurred while creating the partition table.")
  , (#{const VIX_E_MNTAPI_OPEN_FAILURE}, "Failed to open DiskSet.")
  , (#{const VIX_E_MNTAPI_VOLUME_NOT_WRITABLE}, "The volume is write-protected.")
  , (#{const VIX_E_NET_HTTP_UNSUPPORTED_PROTOCOL}, "The URL provided uses an unsupported protocol.")
  , (#{const VIX_E_NET_HTTP_URL_MALFORMAT}, "The URL was not properly formatted.")
  , (#{const VIX_E_NET_HTTP_COULDNT_RESOLVE_PROXY}, "Failed to resolve proxy.")
  , (#{const VIX_E_NET_HTTP_COULDNT_RESOLVE_HOST}, "Failed to resolve host.")
  , (#{const VIX_E_NET_HTTP_COULDNT_CONNECT}, "Failed to connect to host or proxy.")
  , (#{const VIX_E_NET_HTTP_HTTP_RETURNED_ERROR}, "Server returned HTTP error code >= 400.")
  , (#{const VIX_E_NET_HTTP_OPERATION_TIMEDOUT}, "Network operation timed out.")
  , (#{const VIX_E_NET_HTTP_SSL_CONNECT_ERROR}, "A problem occurred during the SSL/TLS handshake.")
  , (#{const VIX_E_NET_HTTP_TOO_MANY_REDIRECTS}, "Reached the maximum number of redirects.")
  , (#{const VIX_E_NET_HTTP_TRANSFER}, "Failure sending/receiving network data.")
  , (#{const VIX_E_NET_HTTP_SSL_SECURITY}, "An SSL error occurred.")
  ]
