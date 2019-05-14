Server.default = s = Server.internal;
s.options.numInputBusChannels = 4;
s.options.numOutputBusChannels = 10;
s.options.blockSize = 512;
s.options.memSize = 2097152; // 2Gb
// s.options.numBuffers = 2048;
s.boot;
// s.waitForBoot {
// 	// Instead of s.meter use custom meter panel
// 	EntroServerMeter(s)
// }