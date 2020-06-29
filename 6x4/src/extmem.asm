copy extaddr2 ff
copy extaddr1 fe
copy extaddr0 ff
copy extmem 'H'
adc extaddr2 extaddr2 1
adc extaddr1 extaddr1 0
adc extaddr0 extaddr0 0
copy extmem 'i'
adc extaddr2 extaddr2 1
adc extaddr1 extaddr1 0
adc extaddr0 extaddr0 0
copy extmem 0
copy extaddr2 ff
copy extaddr1 fe
copy extaddr0 ff
copy addr 01
or acc extmem 0
addifzero pc pc 5
or mem acc 0
adc extaddr2 extaddr2 1
adc extaddr1 extaddr1 0
adc extaddr0 extaddr0 0
copy pc 0f
copy pc fe
