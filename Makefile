.PHONY: all
all: generated/ComparableDict.elm

generated/ComparableDict.elm: codegen/Generate.elm codegen/Gen/CustomDict.elm Makefile
	rm -rf generated
	elm-codegen run

codegen/Gen/CustomDict.elm: codegen/helpers/CustomDict.elm
	elm-codegen install
