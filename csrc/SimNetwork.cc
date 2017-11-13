#include <vpi_user.h>
#include <svdpi.h>

#include "device.h"
#include "switch.h"
#include "memoryblade.h"

class NetworkSwitch *netsw = NULL;
class NetworkDevice *netdev = NULL;
class MemoryBlade *memoryblade = NULL;

static inline int euclid(int a, int b)
{
    while (b > 0) {
        int t = b;
        b = a % b;
        a = t;
    }
    return a;
}

extern "C" void network_init(
        const char *devname,
        int rlimit_gbps,
        char *rlimit_inc,
        char *rlimit_period)
{
    int inc = rlimit_gbps, period = 64;
    int gcd = euclid(inc, period);

    *rlimit_inc = inc / gcd;
    *rlimit_period = (period / gcd) - 1;

    //netsw = new NetworkSwitch(devname);
    netdev = new NetworkDevice(random_macaddr());
    memoryblade = new MemoryBlade(netdev);

    printf("network_init\n");

    //netsw->add_device(netdev);
}

extern "C" void network_tick(
        unsigned char out_valid,
        long long     out_data,
        unsigned char out_last,

        unsigned char *in_valid,
        long long     *in_data,
        unsigned char *in_last,

        long long     *macaddr)
{
    //if (!netdev || !netsw) {
    if (!netdev) {
        *out_ready = 0;
        *in_valid = 0;
        *in_data = 0;
        *in_last = 0;
        return;
    }

    netdev->tick(out_valid, out_data, out_last);
    netdev->switch_to_host();

    //netsw->distribute();
    //netsw->switch_to_worker();
    if (netdev->has_out_packet()) {
      network_packet *p = netdev->pop_out_packet();
      memoryblade->handle_packet(p);
    }

    *in_valid = netdev->in_valid();
    *in_data = netdev->in_data();
    *in_last = netdev->in_last();
    *macaddr = netdev->macaddr();
}
