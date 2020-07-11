:start
copy acc, mem[1]
copy mem[0xff], acc
and acc, 0x80
copyifzero pc, :write
copy pc, :halt
:write
copy acc, mem[0xff]
and acc, 0x7f
copy mem[0], acc
copy pc, :start
