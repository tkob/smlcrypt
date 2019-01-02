COMMON = extword8vector.sml pad.sml crypt-sig.sml block-cipher-fn.sml

check: des-test aes-test
	./des-test
	./aes-test

des-test: des-test.sml des-test.mlb des.sml des.mlb $(COMMON) mlb-path-map
	mlton -mlb-path-map mlb-path-map des-test.mlb

aes-test: aes-test.sml aes-test.mlb aes.sml aes.mlb $(COMMON) mlb-path-map
	mlton -mlb-path-map mlb-path-map aes-test.mlb

clean:
	rm -f des-test
	rm -rf .cm/
