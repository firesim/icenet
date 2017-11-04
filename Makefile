CFLAGS=-Wall -O2 -g -fPIC -std=c++11 -I$(RISCV)/include -D__STDC_FORMAT_MACROS
BUILD ?= build

objs := device.o switch.o memoryblade.o
obj-paths := $(addprefix $(BUILD)/,$(objs))

$(BUILD)/libicenet.so: $(obj-paths)
	mkdir -p $(BUILD)
	$(CXX) -shared $(obj-paths) -o $@

$(BUILD)/%.o: csrc/%.cc
	mkdir -p $(BUILD)
	$(CXX) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(BUILD)/*.o $(BUILD)/*.so
