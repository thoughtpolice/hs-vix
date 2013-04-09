#ifndef __HS_VIX__
#define __HS_VIX__

#include <stdlib.h>
#include "vix.h"

/** Connecting/disconnecting from remote hosts */
VixHandle hs_vix_connect(const char*, const char*, const char*, int,
			 VixServiceProvider, VixError*);
void      hs_vix_disconnect(VixHandle);

/** Opening/closing VMs */
VixHandle hs_vix_vm_open(VixHandle, const char*, VixError*);
void      hs_vix_vm_close(VixHandle);

/** Powering on/off */
int hs_vix_vm_poweron(VixHandle, VixVMPowerOpOptions, VixError*);
int hs_vix_vm_poweroff(VixHandle, VixError*);

#endif /* __HS_VIX__ */
