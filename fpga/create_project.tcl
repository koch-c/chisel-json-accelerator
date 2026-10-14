# Creates a Vivado project for the Nexys 4 DDR (xc7a100tcsg324-1).
#
# GUI:   Vivado -> Tools -> Run Tcl Script... -> select this file
# Batch: vivado -mode batch -source fpga/create_project.tcl              (project only)
#        vivado -mode batch -source fpga/create_project.tcl -tclargs build  (also builds the bitstream)
#
# Run `sbt "runMain GenerateAccelerator"` first. Sources are added by reference,
# so regenerating the Verilog is picked up on the next run without recreating the project.

set root [file normalize [file join [file dirname [info script]] ..]]
set proj_dir [file join $root build vivado]

create_project json_accelerator $proj_dir -part xc7a100tcsg324-1 -force

set top_sv [file join $root generated Accelerator.sv]
if {![file exists $top_sv]} {
  error "$top_sv not found. Run: sbt \"runMain GenerateAccelerator\""
}
add_files -norecurse [list $top_sv]
add_files -fileset constrs_1 -norecurse [list [file join $root Nexys4DDR_Master.xdc]]
set_property top Accelerator [current_fileset]
update_compile_order -fileset sources_1

if {[lsearch $argv build] >= 0} {
  launch_runs impl_1 -to_step write_bitstream -jobs 4
  wait_on_run impl_1
  if {[get_property PROGRESS [get_runs impl_1]] ne "100%"} {
    error "Implementation failed, see $proj_dir/json_accelerator.runs/impl_1/runme.log"
  }
  puts "Bitstream: [glob [file join $proj_dir json_accelerator.runs impl_1 *.bit]]"
}
