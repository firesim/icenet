#include "device.h"

#include <stdio.h>

void NetworkDevice::host_thread(void *arg)
{
    NetworkDevice *netdev = static_cast<NetworkDevice*>(arg);
    netdev->run();

    while (true)
        netdev->target->switch_to();
}

NetworkDevice::NetworkDevice(uint64_t macaddr)
{
    _macaddr = macaddr;
    target = context_t::current();
    host.init(host_thread, this);
}

NetworkDevice::~NetworkDevice()
{
}

void NetworkDevice::run(void)
{
    network_packet *send_packet = new network_packet;
    network_packet *recv_packet;

    init_network_packet(send_packet);

    while (true) {
        while (!out_flits.empty()) {
            assert(send_packet->len < ETH_MAX_WORDS);
            network_packet_add(send_packet, out_flits.front().data);
            if (out_flits.front().last) {
                out_packets.push(send_packet);
                send_packet = new network_packet;
                init_network_packet(send_packet);
            }
            out_flits.pop();
        }

        while (!in_packets.empty()) {
            recv_packet = in_packets.front();
            for (int i = 0; i < recv_packet->len; i++) {
                network_flit flt;
                flt.data = recv_packet->data[i];
                flt.last = (i + 1) == recv_packet->len;
                in_flits.push(flt);
            }
            in_packets.pop();
            delete recv_packet;
        }

        target->switch_to();
    }
}


void NetworkDevice::tick(
            bool out_valid,
            uint64_t out_data,
            bool out_last)
{
    if (out_valid) {
        struct network_flit flt;
        flt.data = out_data;
        flt.last = out_last;
        out_flits.push(flt);
    }

    if (in_valid()) {
        in_flits.pop();
    }
}
