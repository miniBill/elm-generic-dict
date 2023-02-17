.PHONY: all
all: docs.json generated/ComparableDict.elm

docs.json: src/GenericDict.elm
	cp -r codegen/Gen src; lamdera make --docs=docs.json; rm -r src/Gen

generated/ComparableDict.elm: codegen/Generate.elm src/GenericDict.elm codegen/Gen/Basics.elm Makefile
	rm -rf generated
	elm-codegen run

codegen/Gen/Basics.elm: codegen/elm.codegen.json Makefile
	elm-codegen install
