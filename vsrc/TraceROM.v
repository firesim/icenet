import "DPI-C" function int trace_rom_init(
    input string filename
);

import "DPI-C" function void trace_rom_tick(
    output bit     stream_valid,
    input  bit     stream_ready,
    output longint stream_data,
    output byte    stream_keep,
    output bit     stream_last
);

module TraceROM (
    input         clock,
    input         reset,
    output        stream_valid,
    input         stream_ready,
    output [63:0] stream_bits_data,
    output [7:0]  stream_bits_keep,
    output        stream_bits_last,
    output [47:0] macAddr,
    output [31:0] length
);

    bit     __stream_valid;
    longint __stream_data;
    byte    __stream_keep;
    bit     __stream_last;

    reg        __stream_valid_reg;
    reg [63:0] __stream_data_reg;
    reg [7:0]  __stream_keep_reg;
    reg        __stream_last_reg;

    string fname;
    longint __macAddr;
    int     __length;

    reg [47:0] macAddr_reg;
    reg [31:0] length_reg;

    assign macAddr = macAddr_reg;
    assign length  = length_reg;

    assign stream_valid = __stream_valid_reg;
    assign stream_bits_data = __stream_data_reg;
    assign stream_bits_keep = __stream_keep_reg;
    assign stream_bits_last = __stream_last_reg;

    initial begin
        if ($value$plusargs("trace=%s", fname)) begin
            __length = trace_rom_init(fname);
            length_reg = __length;
        end
        if ($value$plusargs("macaddr=%x", __macAddr)) begin
            macAddr_reg = __macAddr;
        end
    end

    always @(posedge clock) begin
        if (reset) begin
            __stream_valid = 0;
            __stream_data  = 0;
            __stream_keep  = 0;
            __stream_last  = 0;

            __stream_valid_reg <= 0;
            __stream_data_reg  <= 0;
            __stream_keep_reg  <= 0;
            __stream_last_reg  <= 0;
        end else begin
            trace_rom_tick(
                __stream_valid,
                stream_ready,
                __stream_data,
                __stream_keep,
                __stream_last);

            __stream_valid_reg <= __stream_valid;
            __stream_data_reg  <= __stream_data;
            __stream_keep_reg  <= __stream_keep;
            __stream_last_reg  <= __stream_last;
        end
    end
endmodule
