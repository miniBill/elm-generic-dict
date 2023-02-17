.PHONY: all
all: docs.json generated/ComparableDict.elm

docs.json: src/GenericDict.elm Makefile
	lamdera make --docs=$@

generated/ComparableDict.elm: codegen/Generate.elm src/GenericDict.elm src/Gen/Basics.elm Makefile
	rm -rf generated
	elm-codegen run

src/Gen/Basics.elm: codegen/elm.codegen.json Makefile
	rm -rf src/Gen
	elm-codegen install
	mv codegen/Gen src
