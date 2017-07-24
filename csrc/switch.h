#ifndef __ICENET_SWITCH_H__
#define __ICENET_SWITCH_H__

#include <vector>

#include "device.h"

class NetworkSwitch {
  public:
    NetworkSwitch(const char *ifname);
    ~NetworkSwitch();

    void add_device(NetworkDevice *dev) { devices.push_back(dev); }
    void distribute(void);
    void switch_to_worker(void) { worker.switch_to(); }

  private:
    int fd;
    std::vector<NetworkDevice*> devices;
    std::queue<network_packet*> out_packets;
    std::queue<network_packet*> in_packets;

    static void worker_thread(void *arg);
    void run(void);

    void broadcast(network_packet *packet, uint64_t skipmac);
    int route(network_packet *packet, uint64_t dstmac);

    context_t* main;
    context_t worker;
};

#endif
