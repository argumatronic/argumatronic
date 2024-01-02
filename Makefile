all: build

CMD=stack exec -- site

stack-build:
	stack build

clean:
	${CMD} clean

build:
	${CMD} build

watch: clean
	${CMD} watch
