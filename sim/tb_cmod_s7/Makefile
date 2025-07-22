# Description: Makefile for simulating the tb_cmod_s7.vhd file.

# List of source files
SRC += $(SRC_DIR)/clock_synthetic.vhd
SRC += $(SRC_DIR)/cmd.vhd
SRC += $(SRC_DIR)/cmod_s7.vhd
SRC += $(SRC_DIR)/line_buffer.vhd
SRC += $(SRC_DIR)/uart_serdes.vhd
SRC += $(SRC_DIR)/uart_wbus.vhd
SRC += $(SRC_DIR)/wbus_csr.vhd
SRC += $(SRC_DIR)/wbus_drp.vhd
SRC += $(SRC_DIR)/wbus_drp.vhd
SRC += $(SRC_DIR)/wbus_mapper.vhd
SRC += $(SRC_DIR)/wbus_pkg.vhd
SRC += $(SRC_DIR)/wbus_pwm.vhd
SRC += $(SRC_DIR)/wbus_xadc.vhd

# List of test specific simulation files
SRC += tb_cmod_s7.vhd

# List of generic simulation files
SRC += $(SIM_SRC_DIR)/tb_kwrds_p.vhd
SRC += $(SIM_SRC_DIR)/tb_parse_p.vhd
SRC += $(SIM_SRC_DIR)/mcu_sim.vhd

# Top level entity
DUT = cmod_s7

# Test duration
STOP_TIME = 500us

# Test specific generics
GENERIC += -gG_SCRIPT_NAME=cmod_s7.in

include ../Makefile.inc

