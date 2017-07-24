#ifndef __ICENET_PACKET_H__
#define __ICENET_PACKET_H__

#include <string.h>

#define NET_IP_ALIGN 2
#define ETH_MAX_WORDS 190
#define ETH_MAX_BYTES 1518
#define MAC_ADDR_BYTES 6
#define BCAST_MAC 0xffffffffffffL

struct network_flit {
    uint64_t data;
    bool last;
};

struct network_packet {
	uint64_t data[ETH_MAX_WORDS];
	int len;
};

static inline void init_network_packet(struct network_packet *packet)
{
    packet->len = 0;
    memset(packet->data, 0, ETH_MAX_WORDS * sizeof(uint64_t));
}

static inline void network_packet_add(network_packet *packet, uint64_t data)
{
    packet->data[packet->len] = data;
    packet->len++;
}

static inline uint64_t network_packet_dstmac(network_packet *packet)
{
    uint64_t dstmac = 0;

    memcpy(&dstmac, ((char *) packet->data) + NET_IP_ALIGN, MAC_ADDR_BYTES);
    return dstmac;
}

static inline network_packet *network_packet_copy(network_packet *packet)
{
    network_packet *packet_copy = new network_packet;
    packet_copy->len = packet->len;
    memcpy(packet_copy->data, packet->data, packet->len * sizeof(uint64_t));
    return packet_copy;
}

#endif
