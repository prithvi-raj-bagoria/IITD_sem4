# Compiler and flags
OCAMLC = ocamlc
OCAMLLEX = ocamllex
OCAMLYACC = ocamlyacc

# Source files
LEXER = lexer.mll
PARSER = parser.mly
AST = ast.ml
TYPECHECKER = typechecker.ml
EVAL = eval.ml
MAIN = main.ml

# Generated files
LEXER_ML = lexer.ml
PARSER_ML = parser.ml
PARSER_MLI = parser.mli

# Compiled files
AST_CMO = ast.cmo
PARSER_CMO = parser.cmo
PARSER_CMI = parser.cmi
LEXER_CMO = lexer.cmo
TYPECHECKER_CMO = typechecker.cmo
EVAL_CMO = eval.cmo
MAIN_CMO = main.cmo

# Executable
EXEC = dsl

# Default target
all: $(EXEC)

# Generate lexer and parser
$(LEXER_ML): $(LEXER)
	$(OCAMLLEX) $(LEXER)

$(PARSER_ML) $(PARSER_MLI): $(PARSER)
	$(OCAMLYACC) $(PARSER)

# Compile modules
$(AST_CMO): $(AST)
	$(OCAMLC) -c $(AST)

$(PARSER_CMI): $(PARSER_MLI)
	$(OCAMLC) -c $(PARSER_MLI)

$(PARSER_CMO): $(PARSER_ML) $(PARSER_CMI)
	$(OCAMLC) -c $(PARSER_ML)

$(LEXER_CMO): $(LEXER_ML)
	$(OCAMLC) -c $(LEXER_ML)

$(TYPECHECKER_CMO): $(TYPECHECKER)
	$(OCAMLC) -c $(TYPECHECKER)

$(EVAL_CMO): $(EVAL)
	$(OCAMLC) -c $(EVAL)

$(MAIN_CMO): $(MAIN)
	$(OCAMLC) -c $(MAIN)

# Link all compiled files into the executable
$(EXEC): $(AST_CMO) $(PARSER_CMO) $(LEXER_CMO) $(TYPECHECKER_CMO) $(EVAL_CMO) $(MAIN_CMO)
	$(OCAMLC) -o $(EXEC) $(AST_CMO) $(PARSER_CMO) $(LEXER_CMO) $(TYPECHECKER_CMO) $(EVAL_CMO) $(MAIN_CMO)

# Clean up generated files
clean:
	rm -f $(LEXER_ML) $(PARSER_ML) $(PARSER_MLI) *.cmo *.cmi $(EXEC)

# Phony targets
.PHONY: all clean