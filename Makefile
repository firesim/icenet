CFLAGS=-Wall -O2 -fPIC -std=c++11 -I$(RISCV)/include

objs := device.o switch.o
obj-paths := $(addprefix build/,$(objs))

build/libicenet.so: $(obj-paths)
	$(CXX) -shared $(obj-paths) -o $@

build/%.o: csrc/%.cc
	$(CXX) $(CFLAGS) -c $< -o $@
