# Nexys 4 DDR constraints for the Uart echo top level.
# Use this file instead of Nexys4DDR_Master.xdc when building Uart.

# 100 MHz board clock
set_property -dict { PACKAGE_PIN E3 IOSTANDARD LVCMOS33 } [get_ports { clock }]
create_clock -name sys_clk_pin -period 10.00 [get_ports { clock }]

# Center button: active-high reset
set_property -dict { PACKAGE_PIN N17 IOSTANDARD LVCMOS33 } [get_ports { reset }]

# USB-UART bridge TX -> FPGA RX; FPGA TX -> bridge RX
set_property -dict { PACKAGE_PIN C4 IOSTANDARD LVCMOS33 } [get_ports { io_uartRx }]
set_property -dict { PACKAGE_PIN D4 IOSTANDARD LVCMOS33 } [get_ports { io_uartTx }]

# Configuration voltage
set_property CFGBVS VCCO [current_design]
set_property CONFIG_VOLTAGE 3.3 [current_design]
