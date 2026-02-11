Q = @    # delete @ for verbosity
SRC = src
BUILD = build

all: top

$(BUILD):
	$(Q)mkdir -p $(BUILD)

top: $(BUILD)
	$(Q)cd $(SRC) && ocamlyacc parser.mly
	$(Q)cd $(SRC) && ocamllex lexer.mll

	$(Q)ocamlc -I $(SRC) -I $(BUILD) -c $(SRC)/types.ml    -o $(BUILD)/types.cmo
	$(Q)ocamlc -I $(SRC) -I $(BUILD) -c $(SRC)/terms.ml    -o $(BUILD)/terms.cmo
	$(Q)ocamlc -I $(SRC) -I $(BUILD) -c $(SRC)/context.ml  -o $(BUILD)/context.cmo
	$(Q)ocamlc -I $(SRC) -I $(BUILD) -c $(SRC)/typing.ml   -o $(BUILD)/typing.cmo
	$(Q)ocamlc -I $(SRC) -I $(BUILD) -c $(SRC)/eval.ml     -o $(BUILD)/eval.cmo

	$(Q)ocamlc -I $(SRC) -I $(BUILD) -c $(SRC)/lambda.mli  -o $(BUILD)/lambda.cmi
	$(Q)ocamlc -I $(SRC) -I $(BUILD) -c $(SRC)/lambda.ml   -o $(BUILD)/lambda.cmo

	$(Q)ocamlc -I $(SRC) -I $(BUILD) -c $(SRC)/parser.mli  -o $(BUILD)/parser.cmi
	$(Q)ocamlc -I $(SRC) -I $(BUILD) -c $(SRC)/parser.ml   -o $(BUILD)/parser.cmo

	$(Q)ocamlc -I $(SRC) -I $(BUILD) -c $(SRC)/lexer.ml    -o $(BUILD)/lexer.cmo
	$(Q)ocamlc -I $(SRC) -I $(BUILD) -c $(SRC)/main.ml     -o $(BUILD)/main.cmo

	$(Q)ocamlc -o top \
		$(BUILD)/types.cmo \
		$(BUILD)/terms.cmo \
		$(BUILD)/context.cmo \
		$(BUILD)/typing.cmo \
		$(BUILD)/eval.cmo \
		$(BUILD)/lambda.cmo \
		$(BUILD)/parser.cmo \
		$(BUILD)/lexer.cmo \
		$(BUILD)/main.cmo

clean:
	$(Q)rm -rf $(BUILD)
	$(Q)rm -f $(SRC)/parser.ml $(SRC)/parser.mli $(SRC)/lexer.ml
	$(Q)rm top
