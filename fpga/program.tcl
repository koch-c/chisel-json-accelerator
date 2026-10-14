# Programs the Nexys 4 DDR over USB-JTAG with the bitstream from create_project.tcl.
# Usage: vivado -mode batch -source fpga/program.tcl

set root [file normalize [file join [file dirname [info script]] ..]]
set bit [file join $root build vivado json_accelerator.runs impl_1 Accelerator.bit]

open_hw_manager
connect_hw_server
open_hw_target
set dev [lindex [get_hw_devices xc7a100t*] 0]
current_hw_device $dev
set_property PROGRAM.FILE $bit $dev
program_hw_devices $dev
close_hw_manager
