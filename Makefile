COMMON = extword8vector.sml pad.sml crypt-sig.sml block-cipher-fn.sml

check: des-test aes-test rsa-test
	./des-test
	./aes-test
	./rsa-test

des-test: des-test.sml des-test.mlb des.sml des.mlb $(COMMON) mlb-path-map
	mlton -mlb-path-map mlb-path-map des-test.mlb

aes-test: aes-test.sml aes-test.mlb aes.sml aes.mlb $(COMMON) mlb-path-map
	mlton -mlb-path-map mlb-path-map aes-test.mlb

rsa-test: rsa-test.sml rsa-test.mlb rsa.sml rsa.mlb $(COMMON) mlb-path-map
	mlton -mlb-path-map mlb-path-map rsa-test.mlb

clean:
	rm -f des-test aes-test rsa-test
	rm -rf .cm/
