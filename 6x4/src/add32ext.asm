# *(DWORD*)0x010000 = 0xffffffff
# *(DWORD*)0x020000 = 0x01020302
# *(DWORD*)0x030000 = *(DWORD*)0x010000 + *(DWORD*)0x020000
# assum DWORD aligned
# load 0x01020304 @ 0x010000
copy extaddr2 00
copy extaddr1 00
copy extaddr0 01
copy extmem ff
adc extaddr2 extaddr2 1
copy extmem 3
adc extaddr2 extaddr2 1
copy extmem 2
adc extaddr2 extaddr2 1
copy extmem 1
# load 0x01020304 @ 0x100000
copy extaddr2 00
copy extaddr1 00
copy extaddr0 02
copy extmem 4
adc extaddr2 extaddr2 1
copy extmem 3
adc extaddr2 extaddr2 1
copy extmem 2
adc extaddr2 extaddr2 1
copy extmem 1
# begin
# first clear carry flag and set up destination
copy acc 0
adc acc acc 0
# add byte 0
copy extaddr2 00
copy extaddr1 00
copy extaddr0 01
adc acc extmem 0
copy extaddr2 00
copy extaddr1 00
copy extaddr0 02
adc acc acc extmem
copy extaddr2 00
copy extaddr1 00
copy extaddr0 03
or extmem acc 0
# add byte 1
copy extaddr2 01
copy extaddr1 00
copy extaddr0 01
adc acc extmem 0
copy extaddr2 01
copy extaddr1 00
copy extaddr0 02
adc acc acc extmem
copy extaddr2 01
copy extaddr1 00
copy extaddr0 03
or extmem acc 0
# add byte 2
copy extaddr2 02
copy extaddr1 00
copy extaddr0 01
adc acc extmem 0
copy extaddr2 02
copy extaddr1 00
copy extaddr0 02
adc acc acc extmem
copy extaddr2 02
copy extaddr1 00
copy extaddr0 03
or extmem acc 0
# add byte 3
copy extaddr2 03
copy extaddr1 00
copy extaddr0 01
adc acc extmem 0
copy extaddr2 03
copy extaddr1 00
copy extaddr0 02
adc acc acc extmem
copy extaddr2 03
copy extaddr1 00
copy extaddr0 03
or extmem acc 0
copy pc fe
