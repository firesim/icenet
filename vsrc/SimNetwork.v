import "DPI-C" function void network_tick
(
    input  bit     out_valid,
    output bit     out_ready,
    input  longint out_data,
    input  bit     out_last,

    output bit     in_valid,
    input  bit     in_ready,
    output longint in_data,
    output bit     in_last,

    output longint macaddr
);

import "DPI-C" function void network_init(
    input string devname,
    input int rlimit_gbps,
    output byte rlimit_inc,
    output byte rlimit_period
);

module SimNetwork(
    input         clock,
    input         reset,

    input         net_out_valid,
    output        net_out_ready,
    input  [63:0] net_out_bits_data,
    input  [7:0]  net_out_bits_keep,
    input         net_out_bits_last,

    output        net_in_valid,
    input         net_in_ready,
    output [63:0] net_in_bits_data,
    output [7:0]  net_in_bits_keep,
    output        net_in_bits_last,

    output [47:0] net_macAddr,
    output [7:0]  net_rlimit_inc,
    output [7:0]  net_rlimit_period,
    output [7:0]  net_rlimit_size
);

    string devname = "";
    int rlimit_gbps = 64;
    byte rlimit_inc = 1;
    byte rlimit_period = 1;
    byte rlimit_size = 8;
    int dummy;

    bit __out_ready;
    bit __in_valid;
    longint __in_data;
    bit __in_last;
    longint __macaddr;

    reg        __out_ready_reg;
    reg        __in_valid_reg;
    reg [63:0] __in_data_reg;
    reg        __in_last_reg;
    reg [47:0] __macaddr_reg;

    initial begin
        dummy = $value$plusargs("netbw=%d", rlimit_gbps);
        dummy = $value$plusargs("netburst=%d", rlimit_size);
        dummy = $value$plusargs("netdev=%s", devname);
        network_init(devname, rlimit_gbps, rlimit_inc, rlimit_period);
    end

    /* verilator lint_off WIDTH */
    always @(posedge clock) begin
        if (reset) begin
            __out_ready = 0;
            __in_valid = 0;
            __in_data = 0;
            __in_last = 0;

            __out_ready_reg <= 1'b0;
            __in_valid_reg <= 1'b0;
            __in_data_reg <= 64'b0;
            __in_last_reg <= 1'b0;
        end else begin
            network_tick(
                net_out_valid,
                __out_ready,
                net_out_bits_data,
                net_out_bits_last,

                __in_valid,
                net_in_ready,
                __in_data,
                __in_last,

                __macaddr);

            __out_ready_reg <= __out_ready;
            __in_valid_reg <= __in_valid;
            __in_data_reg <= __in_data;
            __in_last_reg <= __in_last;
            __macaddr_reg <= __macaddr;
        end
    end

    assign net_out_ready = __out_ready_reg;
    assign net_in_valid = __in_valid_reg;
    assign net_in_bits_data = __in_data_reg;
    assign net_in_bits_keep = 8'hff;
    assign net_in_bits_last = __in_last_reg;
    assign net_macAddr = __macaddr_reg;
    assign net_rlimit_inc = rlimit_inc;
    assign net_rlimit_period = rlimit_period;
    assign net_rlimit_size = rlimit_size;

endmodule
