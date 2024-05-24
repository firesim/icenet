#include "switch.h"

#include <inttypes.h>

#include <arpa/inet.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/select.h>
#include <sys/time.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>

#include <linux/if.h>
#include <linux/if_tun.h>

static int tuntap_alloc(const char *dev, int flags)
{
	struct ifreq ifr;
	int fd, err;

	if ((fd = open("/dev/net/tun", O_RDWR)) < 0) {
		perror("open()");
		return fd;
	}

	memset(&ifr, 0, sizeof(ifr));

	ifr.ifr_flags = flags;
	strncpy(ifr.ifr_name, dev, IFNAMSIZ);

	if ((err = ioctl(fd, TUNSETIFF, &ifr)) < 0) {
		perror("ioctl()");
		close(fd);
		return err;
	}

	return fd;
}

NetworkSwitch::NetworkSwitch(const char *ifname)
{
    if (ifname == NULL || strlen(ifname) == 0) {
        fd = -1;
        goto skip_tuntap;
    }

    fd = tuntap_alloc(ifname, IFF_TAP | IFF_NO_PI);
    if (fd < 0) {
        fprintf(stderr, "Could not open tap interface\n");
        abort();
    }

skip_tuntap:
    main = context_t::current();
    worker.init(worker_thread, this);
}

NetworkSwitch::~NetworkSwitch()
{
    if (fd != -1)
        close(fd);
}

void NetworkSwitch::worker_thread(void *arg)
{
    NetworkSwitch *sw = static_cast<NetworkSwitch*>(arg);
    sw->run();

    while (true)
        sw->main->switch_to();
}

#define ceil_div(n, d) (((n) - 1) / (d) + 1)

void NetworkSwitch::run(void)
{
    fd_set rfds, wfds;
    struct timeval timeout;
    int retval;

    if (fd == -1) {
        while (true)
            main->switch_to();
        return;
    }

    timeout.tv_sec = 1;
    timeout.tv_usec = 0;

    FD_ZERO(&rfds);
    FD_ZERO(&wfds);

    while (true) {
        FD_SET(fd, &rfds);
        FD_SET(fd, &wfds);

        retval = select(fd + 1, &rfds, &wfds, NULL, &timeout);

        if (retval < 0) {
            perror("select()");
            abort();
        }

        if (retval == 0) {
            main->switch_to();
            continue;
        }

        if (FD_ISSET(fd, &rfds)) {
            network_packet *recv_packet;
            char *recv_buffer;

            recv_packet = new network_packet;
            init_network_packet(recv_packet);
            recv_buffer = ((char *) recv_packet->data) + NET_IP_ALIGN;

            retval = read(fd, recv_buffer, ETH_MAX_BYTES);
            if (retval < 0) {
                perror("read()");
                abort();
            }
            recv_packet->len = ceil_div(retval + NET_IP_ALIGN, sizeof(uint64_t));
            in_packets.push(recv_packet);
        }

        if (FD_ISSET(fd, &wfds) && !out_packets.empty()) {
            network_packet *send_packet;
            char *send_buffer;
            size_t nbytes;

            send_packet = out_packets.front();
            send_buffer = ((char *) send_packet->data) + NET_IP_ALIGN;
            nbytes = send_packet->len * sizeof(uint64_t) - NET_IP_ALIGN;

            retval = write(fd, send_buffer, nbytes);
            if (retval < 0) {
                perror("write()");
                abort();
            }

            out_packets.pop();
            delete send_packet;
        }

        main->switch_to();
    }
}

void NetworkSwitch::broadcast(network_packet *packet, uint64_t skipmac)
{
    for (auto dev : devices) {
        if (dev->macaddr() == skipmac)
            continue;
        network_packet *packet_copy = network_packet_copy(packet);
        dev->push_in_packet(packet_copy);
    }
}

int NetworkSwitch::route(network_packet *packet, uint64_t dstmac)
{
    for (auto dev : devices) {
        if (dev->macaddr() == dstmac) {
            dev->push_in_packet(packet);
            return 0;
        }
    }
    return -1;
}

void NetworkSwitch::distribute(void)
{
    while (!in_packets.empty()) {
        network_packet *packet = in_packets.front();
        uint64_t dstmac = network_packet_dstmac(packet);

        if (dstmac == BCAST_MAC) {
            broadcast(packet, 0);
            delete packet;
        } else if (route(packet, dstmac)) {
            fprintf(stderr, "Dropped packet for %" PRIx64 "\n", dstmac);
            delete packet;
        }
        in_packets.pop();
    }

    for (auto dev : devices) {
        if (dev->has_out_packet()) {
            network_packet *packet = dev->pop_out_packet();
            uint64_t dstmac = network_packet_dstmac(packet);

            if (dstmac == BCAST_MAC) {
                broadcast(packet, dev->macaddr());
                delete packet;
            } else if (route(packet, dstmac)) {
                out_packets.push(packet);
            }
        }
    }
}
