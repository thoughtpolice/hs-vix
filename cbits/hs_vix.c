#include "hs_vix.h"

VixHandle
hs_vix_connect(const char* host, const char* user, const char* pass, int port,
	       VixServiceProvider conntype)
{
  VixError err;
  
  VixHandle jobHandle  = VIX_INVALID_HANDLE;
  VixHandle hostHandle = VIX_INVALID_HANDLE;

  jobHandle = VixHost_Connect(VIX_API_VERSION, conntype,
			      host, port,
			      user, pass,
			      0, VIX_INVALID_HANDLE,
			      NULL, NULL);
  err = VixJob_Wait(jobHandle, 
		    VIX_PROPERTY_JOB_RESULT_HANDLE, 
		    &hostHandle,
		    VIX_PROPERTY_NONE);
  if (VIX_FAILED(err)) {
    goto abort;
  }

  Vix_ReleaseHandle(jobHandle);
  return hostHandle;

 abort:
  Vix_ReleaseHandle(jobHandle);
  Vix_ReleaseHandle(hostHandle);
  return VIX_INVALID_HANDLE;
}

void
hs_vix_disconnect(VixHandle hdl)
{
  VixHost_Disconnect(hdl);
}


VixHandle
hs_vix_vm_open(VixHandle host, const char* vmxpath)
{
  VixError err;
  VixHandle jobHandle = VIX_INVALID_HANDLE;
  VixHandle vmHandle  = VIX_INVALID_HANDLE;

  jobHandle = VixHost_OpenVM(host, vmxpath,
			     VIX_VMOPEN_NORMAL, VIX_INVALID_HANDLE,
			     NULL, NULL);
  err = VixJob_Wait(jobHandle, 
		    VIX_PROPERTY_JOB_RESULT_HANDLE, 
		    &vmHandle,
		    VIX_PROPERTY_NONE);
  if (VIX_FAILED(err)) {
    goto abort;
  }

  Vix_ReleaseHandle(jobHandle);
  return vmHandle;

 abort:
  Vix_ReleaseHandle(jobHandle);
  Vix_ReleaseHandle(vmHandle);
  return VIX_INVALID_HANDLE;
}

void
hs_vix_vm_close(VixHandle hdl)
{
  Vix_ReleaseHandle(hdl);
}


int
hs_vix_vm_poweron(VixHandle hdl, VixVMPowerOpOptions poweropts)
{
  int res = 0;
  VixError err;
  VixHandle job = VIX_INVALID_HANDLE;

  job = VixVM_PowerOn(hdl, poweropts, VIX_INVALID_HANDLE, NULL, NULL);
  err = VixJob_Wait(job, VIX_PROPERTY_NONE);
  if (VIX_FAILED(err)) goto abort;
  res = 1; /* Success */

 abort:
  Vix_ReleaseHandle(job);
  return res;
}

int
hs_vix_vm_poweroff(VixHandle hdl)
{
  int res = 0;
  VixError err;
  VixHandle job = VIX_INVALID_HANDLE;

  job = VixVM_PowerOff(hdl, VIX_VMPOWEROP_NORMAL, NULL, NULL);
  err = VixJob_Wait(job, VIX_PROPERTY_NONE);
  if (VIX_FAILED(err)) goto abort;
  res = 1; /* Success */

 abort:
  Vix_ReleaseHandle(job);
  return res;
}
