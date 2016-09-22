LYCOHOME = schema
STACK = stack
LYCO = ${STACK} exec lyco --
XDG_DATA_HOME := $(shell pwd)/.stack-work/docker/_home/.local/share
XDG_CONFIG_HOME := $(shell pwd)/.stack-work/docker/_home/.config

.PHONY: all quick bench clean veryclean install sdist init configure start

all: build

client-build:
	mkdir -p assets
	(cd client ; make)

build: client-build
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

start: build
	@XDG_DATA_HOME="${XDG_DATA_HOME}" XDG_CONFIG_HOME="${XDG_CONFIG_HOME}" ${LYCO} -- start -p 8080 -d "./assets"

