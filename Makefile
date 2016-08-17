LYCOHOME = schema
STACK = stack
LYCO = ${STACK} exec lyco --
XDG_DATA_HOME := $(shell pwd)/.local/share
XDG_CONFIG_HOME := $(shell pwd)/.config

.PHONY: all quick bench clean veryclean install sdist init configure

all: build

build:
	@${STACK} build

watch-build:
	@${STACK} build --file-watch

test:
	@${STACK} test

clean:
	@${STACK} clean
	@rm -rf ${XDG_DATA_HOME}/lycopene/issues.db

version: build
	@XDG_DATA_HOME="${XDG_DATA_HOME}" XDG_CONFIG_HOME="${XDG_CONFIG_HOME}" ${LYCO} -- version

configure: build
	@XDG_DATA_HOME="${XDG_DATA_HOME}" XDG_CONFIG_HOME="${XDG_CONFIG_HOME}" ${LYCO} -- configure

project: build
	@XDG_DATA_HOME="${XDG_DATA_HOME}" XDG_CONFIG_HOME="${XDG_CONFIG_HOME}" ${LYCO} -- project

