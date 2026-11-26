We are building a hardware accelerated JSON parser in Chisel.

The FPGA board we are developing on is a Nexys 4 DDR. 

Do not make any changes to README unless explicitly told so.

`AcceleratorTop` serves as a simple UART wrapper of `Accelerator`. It exists because then `Accelerator` can be tested independently of the slow UART simulation. Therefore, `AcceleratorTopSpec` should contain a minimal suit of smoke tests, while `AcceleratorSpec` should contain the bulk of the integration tests for the accelerator. 
