#ifndef __ICENET_MEMORYBLADE_H_
#define __ICENET_MEMORYBLADE_H_

#include <unordered_map>
#include <cassert>
#include "packet.h"
#include "device.h"

struct remotepage {
  uint64_t frag1[ETH_MAX_WORDS] = {0};
	uint16_t frag1len;
  uint64_t frag2[ETH_MAX_WORDS] = {0};
	uint16_t frag2len;
  uint64_t frag3[ETH_MAX_WORDS] = {0};
	uint16_t frag3len;
};

struct remotepage_header {
  //uint8_t version;
  uint8_t opcode;
  uint8_t partid;
  uint32_t pageid;
  uint16_t xactid;

  remotepage_header(uint64_t data) {
    opcode = data >> 56;
    partid = (data >> 48) & 0xff;
    pageid = (data >> 16) & 0xffffffff;
    xactid = data & 0xffff;
  }
};


class MemoryBlade {
public:
	MemoryBlade(NetworkDevice *netdev);
	~MemoryBlade();
  void handle_packet(network_packet *packet);

private:
	std::unordered_map<uint32_t, remotepage *> memory_map;
	NetworkDevice *_netdev;

  void write(uint32_t pageid, uint8_t fragid, char *packetbuf, size_t bytes);
  void read(uint32_t pageid);
  network_packet *build_packet(char *pagebuf, size_t bytes);
};

#endif
