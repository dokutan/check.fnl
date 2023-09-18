.PHONY:  test clean

check.fnl:
	echo "#!/usr/bin/env lua" > check.fnl
	cd src; fennel -c --require-as-include check.fnl >> ../check.fnl
	chmod +x check.fnl

test: check.fnl
	cd tests; fennel test.fnl

clean:
	rm check.fnl
