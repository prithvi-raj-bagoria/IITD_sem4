# Makefile for Krivine and SECD machines

# OCaml compiler
OCAMLC = ocamlc

# Target executables
TARGETS = krivine.byte secd.byte

all: $(TARGETS)

# Rules for compiling each target
krivine.byte: krivine.ml
	$(OCAMLC) -o $@ $<

secd.byte: secd.ml
	$(OCAMLC) -o $@ $<

# Clean rule
clean:
	rm -f *.cmo *.cmi $(TARGETS)

# Test rule to run both executables
test: $(TARGETS)
	./krivine.byte
	./secd.byte
