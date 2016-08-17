LYCOHOME = schema
XDG_DATA_HOME := $(shell pwd)/.local/share
XDG_CONFIG_HOME := $(shell pwd)/.config

.PHONY: all quick bench clean veryclean install sdist init configure

all: build

build:
	stack build

watch-build:
	stack build --file-watch

clean:
	stack clean
	rm -rf ${XDG_DATA_HOME}/lycopene/issues.db

configure: build
	XDG_DATA_HOME="${XDG_DATA_HOME}" XDG_CONFIG_HOME="${XDG_CONFIG_HOME}" stack exec lyco -- configure
