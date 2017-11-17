#include "memoryblade.h"

MemoryBlade::MemoryBlade(NetworkDevice *netdev) {
  _netdev = netdev;
}

MemoryBlade::~MemoryBlade() { }

void MemoryBlade::handle_packet(network_packet *packet) {
  remotepage_header header(packet->data[0]);
  printf("memory blade handling packet=%lx, opcode=%d, partid=%d, pageid=%d, xactid=%d\n",
            packet->data[0], header.opcode, header.partid, header.pageid, header.xactid);
  char *packetbuf = (char *) &packet->data[1];
  size_t bytes = (packet->len - 1) * sizeof(uint64_t);
  switch (header.opcode) {
    case 1:
      write(header.pageid, header.partid, packetbuf, bytes);
      break;
    case 0:
      read(header.pageid);
      break;
    default:
      assert(false == true);
  }

  // delete packet;
}

void MemoryBlade::write(uint32_t pageid, uint8_t fragid, char *packetbuf, size_t bytes) {
  printf("store page fragment=%d, pageid=%d bytes=%lu\n", fragid, pageid, bytes);
  remotepage *rp = NULL;
  if (memory_map.count(pageid) == 0) {
    rp = new remotepage;
    memory_map.insert({pageid, rp});
  } else {
    rp = memory_map[pageid];
  }
  char *targetbuf = NULL;
  switch (fragid) {
    case 0:
      targetbuf = (char *) &rp->frag1[0];
      rp->frag1len = bytes / 8;
      printf("selecting frag1, len=%d\n", rp->frag1len);
      break;
    case 1:
      targetbuf = (char *) &rp->frag2[0];
      rp->frag2len = bytes / 8;
      printf("selecting frag2, len=%d\n", rp->frag2len);
      break;
    case 2:
      targetbuf = (char *) &rp->frag3[0];
      rp->frag3len = bytes / 8;
      printf("selecting frag3, len=%d\n", rp->frag3len);
      break;
    default:
      assert(false == true);
  }
  printf("targetbuf=%p, packetbuf[0]=%d\n", targetbuf, packetbuf[0]);
  memcpy(targetbuf, packetbuf, bytes);
}

void MemoryBlade::read(uint32_t pageid) {
  printf("read page pageid=%d\n", pageid);
  assert(memory_map.count(pageid) != 0);

  remotepage *rp = memory_map[pageid];
  _netdev->push_in_packet(build_packet((char *) &rp->frag1[0], rp->frag1len * sizeof(uint64_t)));
  _netdev->push_in_packet(build_packet((char *) &rp->frag2[0], rp->frag2len * sizeof(uint64_t)));
  _netdev->push_in_packet(build_packet((char *) &rp->frag3[0], rp->frag3len * sizeof(uint64_t)));
}

#define ceil_div(n, d) (((n) - 1) / (d) + 1)

network_packet *MemoryBlade::build_packet(char *pagebuf, size_t bytes) {
  printf("building a packet for nic, pagebuf=%p, pagebuf[0]=%d, bytes=%lu\n", pagebuf, pagebuf[0], bytes);
  network_packet *packet = new network_packet;
  char *packetbuf = ((char *)packet->data);
  init_network_packet(packet);
  memcpy(packetbuf, pagebuf, bytes);
  packet->len = ceil_div(bytes, sizeof(uint64_t));
  printf("built a packet for nic, len=%d\n", packet->len);
  return packet;
}
