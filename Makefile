.PHONY: all
all: docs.json generated/IdDict.elm

docs.json: src/GenericDict.elm Makefile
	lamdera make --docs=$@

generated/IdDict.elm: codegen/Generate.elm src/GenericDict.elm src/Gen/Basics.elm Makefile
	rm -rf generated
	elm-codegen run

src/Gen/Basics.elm: codegen/elm.codegen.json Makefile
	rm -rf src/Gen
	elm-codegen install
	mv codegen/Gen src
	mkdir -p codegen/Gen
	mv src/Gen/CodeGen codegen/Gen/
