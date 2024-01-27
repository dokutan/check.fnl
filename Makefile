.PHONY: binary test clean

LUA_LIB="/usr/lib64/liblua.so"
LUA_DIR="/usr/include/lua"

check.fnl: src/*
	echo "#!/usr/bin/env lua" > $@
	cd src; fennel -c --require-as-include check.fnl >> ../$@
	chmod +x $@

binary:
	cd src; fennel --compile-binary check.fnl ../check.fnl ${LUA_LIB} ${LUA_DIR}

test: check.fnl
	cd tests; fennel test.fnl

lint: check.fnl
	./check.fnl src/*

clean:
	rm check.fnl
