check: des-test
	./des-test

des-test: des-test.sml des-test.mlb des.sml des.mlb mlb-path-map
	mlton -mlb-path-map mlb-path-map des-test.mlb

clean:
	rm -f des-test
	rm -rf .cm/
