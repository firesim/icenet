#include <vpi_user.h>
#include <svdpi.h>

#include <time.h>
#include <stdlib.h>

#include "device.h"
#include "switch.h"

class NetworkSwitch *netsw = NULL;
class NetworkDevice *netdev = NULL;

extern "C" void network_init(
        const char *devname)
{
    uint64_t macaddr = 0;
    long *macaddr_bits = (long *) &macaddr;
    int i;

    // Generate random MAC according to
    // https://access.redhat.com/documentation/en-US/Red_Hat_Enterprise_Linux/5/html/Virtualization/sect-Virtualization-Tips_and_tricks-Generating_a_new_unique_MAC_address.html
    srandom(time(0));

    for (i = 0; (i * sizeof(long)) < sizeof(uint64_t); i++) {
        macaddr_bits[i] = random();
    }

    macaddr &= 0xffff7f000000;
    macaddr |= 0x3e1600;

    netsw = new NetworkSwitch(devname);
    netdev = new NetworkDevice(macaddr);

    netsw->add_device(netdev);
}

extern "C" void network_tick(
        unsigned char out_valid,
        unsigned char *out_ready,
        long long     out_data,
        unsigned char out_last,

        unsigned char *in_valid,
        unsigned char in_ready,
        long long     *in_data,
        unsigned char *in_last,

        long long     *macaddr)
{
    if (!netdev || !netsw) {
        *out_ready = 0;
        *in_valid = 0;
        *in_data = 0;
        *in_last = 0;
        return;
    }

    netdev->tick(out_valid, out_data, out_last, in_ready);
    netdev->switch_to_host();

    netsw->distribute();
    netsw->switch_to_worker();

    *out_ready = netdev->out_ready();
    *in_valid = netdev->in_valid();
    *in_data = netdev->in_data();
    *in_last = netdev->in_last();
    *macaddr = netdev->macaddr();
}
