# Description: Makefile for simulating

################## Don't change anything below here ******************************

TB = tb_$(DUT)
WAVE = $(TB).ghw
SAVE = $(TB).gtkw

COV = -Wc,-fprofile-arcs -Wc,-ftest-coverage -Wl,-lgcov

COMP_OPTIONS += -frelaxed
COMP_OPTIONS += -fsynopsys

RUN_OPTIONS += --ieee-asserts=disable-at-0
RUN_OPTIONS += --assert-level=error

SRC_DIR = ../../src
SIM_SRC_DIR = ../src

# Specify install location of the AMD Vivado tool
VIVADO_DIR = /opt/Xilinx/Vivado/2024.1
XPM_DIR = ../../../../fransschreuder/xpm_vhdl

PASS: $(TB)
	rm -f PASS
	rm -f *.gcda
	ghdl -r --std=08 $(TB) $(GENERIC) $(RUN_OPTIONS) --wave=$(WAVE) --stop-time=$(STOP_TIME)
	echo > PASS

.PHONY: show_coverage
show_coverage: html
	xdg-open html/index.html

$(TB): $(SRC) xpm-obj08.cf unisim-obj08.cf
	ghdl -i --std=08 $(COMP_OPTIONS) $(SRC)
	ghdl -m --std=08 $(COMP_OPTIONS) $(COV) $(TB)

unisim-obj08.cf:
	ghdl -i --std=08 --work=unisim -frelaxed-rules -fsynopsys $(VIVADO_DIR)/data/vhdl/src/unisims/unisim_VPKG.vhd
	ghdl -i --std=08 --work=unisim -frelaxed-rules -fsynopsys $(VIVADO_DIR)/data/vhdl/src/unisims/unisim_retarget_VCOMP.vhd
	ghdl -i --std=08 --work=unisim -frelaxed-rules -fsynopsys $(VIVADO_DIR)/data/vhdl/src/unisims/primitive/*.vhd
	ghdl -i --std=08 --work=unisim -frelaxed-rules -fsynopsys $(VIVADO_DIR)/data/vhdl/src/unisims/retarget/*.vhd

xpm-obj08.cf:
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_VCOMP.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_cdc/hdl/xpm_cdc_single.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_cdc/hdl/xpm_cdc_array_single.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_cdc/hdl/xpm_cdc_async_rst.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_cdc/hdl/xpm_cdc_gray.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_cdc/hdl/xpm_cdc_handshake.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_cdc/hdl/xpm_cdc_low_latency_handshake.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_cdc/hdl/xpm_cdc_pulse.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_cdc/hdl/xpm_cdc_sync_rst.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_memory/hdl/xpm_memory_base.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_memory/hdl/xpm_memory_dpdistram.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_memory/hdl/xpm_memory_dprom.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_memory/hdl/xpm_memory_sdpram.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_memory/hdl/xpm_memory_spram.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_memory/hdl/xpm_memory_sprom.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_memory/hdl/xpm_memory_tdpram.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_fifo/hdl/xpm_fifo_rst.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_fifo/hdl/xpm_fifo_reg_bit.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_fifo/hdl/xpm_counter_updn.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_fifo/hdl/xpm_fifo_reg_vec.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_fifo/hdl/xpm_reg_pipe_bit.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_fifo/hdl/xpm_fifo_base.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_fifo/hdl/xpm_fifo_async.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_fifo/hdl/xpm_fifo_axi_reg_slice.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_fifo/hdl/xpm_fifo_axif.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_fifo/hdl/xpm_fifo_axil.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_fifo/hdl/xpm_fifo_axis.vhd
	ghdl -i --std=08 --work=xpm ${XPM_DIR}/src/xpm/xpm_fifo/hdl/xpm_fifo_sync.vhd


html: $(WAVE)
	rm -f e~$(TB).gcno
	rm -f e~$(TB).gcda
	rm -rf html
	lcov -c -d . -o $(TB)_gcov.info
	genhtml -o html $(TB)_gcov.info

questa: $(SRC)
	vcom -2008 $(SRC)
	vsim -do vsim_$(DUT).do

show: $(WAVE)
	gtkwave $(WAVE) $(SAVE)

clean:
	rm -rf *.o
	rm -rf *.cf
	rm -rf $(TB)
	rm -rf $(WAVE)
	rm -rf $(DUT)_bmc
	rm -rf $(DUT)_cover
	rm -rf work
	rm -rf *.gcda
	rm -rf *.gcno
	rm -rf tb_safety_gcov.info
	rm -rf html
	rm -rf PASS

