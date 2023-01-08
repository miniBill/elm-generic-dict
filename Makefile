.PHONY: all
all: generated/ComparableDict.elm benchmarks/src/ParamDict.elm

generated/ComparableDict.elm: codegen/Generate.elm codegen/Gen/CustomDict.elm Makefile
	rm -rf generated
	elm-codegen run

codegen/Gen/CustomDict.elm: codegen/helpers/CustomDict.elm Makefile
	elm-codegen install

benchmarks/src/ParamDict.elm: codegen/Generate.elm Makefile
	elm-codegen run
	cp generated/ParamDict.elm $@
