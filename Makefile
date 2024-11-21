WFLAGS=-Wfunction-references=y

DEPS=erwasm/minibeam/math.wat \
  erwasm/minibeam/minibeam_mem.wat \
  erwasm/minibeam/minibeam_list.wat \
  erwasm/minibeam/minibeam_eq.wat \
  erwasm/minibeam/minibeam_tuple.wat \
  erwasm/minibeam/shim.wat \
  erwasm/minibeam/minibeam_bs.wat

JSONE=vendor/jsone_decode.wat vendor/jsone_encode.wat

%.S: %.erl
	erlc -DNO_MAP_TYPE -S -o $(shell dirname $<) $<

%.wat: %.S
	python erwasm/erwasmc.py $< $@

%.wasm: %.wat
	wat2wasm $< -o $@

%service.fat.wat: %service.wat $(JSONE) $(DEPS)
	python watcat/watmerge.py $@ $< $(DEPS) $(JSONE)

%.fat.wat: %.wat
	python watcat/watmerge.py $@ $< $(DEPS)

clean:
	rm -f {vendor,esrc}/*.{S,wat,wasm}

all: esrc/price_service.fat.wasm
