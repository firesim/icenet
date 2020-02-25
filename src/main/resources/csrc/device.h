#ifndef __ICENET_DEVICE_H__
#define __ICENET_DEVICE_H__

#include <queue>
#include <stdint.h>
#include <cassert>

#include "fesvr/context.h"
#include "packet.h"

class NetworkDevice {
  public:
    NetworkDevice(uint64_t macaddr);
    ~NetworkDevice();

    void tick(
            bool out_valid,
            uint64_t out_data,
            bool out_last);

    bool in_valid() { return !in_flits.empty(); }
    uint64_t in_data() { return (in_valid()) ? in_flits.front().data : 0; }
    bool in_last() { return (in_valid()) ? in_flits.front().last : false; }
    void switch_to_host(void) { host.switch_to(); }
    void send_out(struct network_flit &flt) { out_flits.push(flt); }
    struct network_flit recv_in(void) {
        struct network_flit flt = in_flits.front();
        in_flits.pop();
        return flt;
    }
    uint64_t macaddr() { return _macaddr; }
    void set_macaddr(uint64_t macaddr) { _macaddr = macaddr; }
    bool has_out_packet(void) { return !out_packets.empty(); }
    network_packet *pop_out_packet(void) {
        network_packet *pkt = out_packets.front();
        out_packets.pop();
        return pkt;
    }
    void push_in_packet(network_packet *packet) { in_packets.push(packet); }

  protected:
    std::queue<network_flit> out_flits;
    std::queue<network_flit> in_flits;

    std::queue<network_packet*> out_packets;
    std::queue<network_packet*> in_packets;

    static void host_thread(void *arg);
    virtual void run(void);

    context_t* target;
    context_t host;

    uint64_t _macaddr;
};

#endif
