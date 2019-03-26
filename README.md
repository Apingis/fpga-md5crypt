Optimization effort on md5crypt-ztex

- Optimized rotation by variable length using low-level primitive ```MUXF7``` (
<a href="https://github.com/Apingis/fpga-md5crypt/blob/ca9caf6c5c6236a8296b5905c90a6d0fe0a478a2/md5crypt/md5core/md5core.v#L680">see here</a>
). This adds extra LUTs from SLICEL/SLICEM slices, allows the signal to pass no more than 2 LUT/cycle, increases frequency reported by the toolset.
- Separated PKT_COMM_CLK clock
- Minor improvements
