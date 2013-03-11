
### PARAMETERS #####################################################

PACKAGES=core
SYNTAX=sexplib
MODULES= std
#MODULES= file1 file2 std

B=foo
T=$B.cmxa

FOR_PACK_OPT=-for-pack Foo

### RULES ##########################################################

SYNTAX_PACKAGES=$(addsuffix .syntax, $(SYNTAX))

PACKAGE_FLAGS=$(addprefix -package , $(SYNTAX_PACKAGES) $(PACKAGES))

FIND_OPTS=$(PACKAGE_FLAGS)
ifdef SYNTAX
  ifneq "$(SYNTAX)" ""
    FIND_OPTS=-syntax camlp4o $(PACKAGE_FLAGS)
  endif
endif

OCAMLOPT_FLAGS=\
  $(FIND_OPTS) \
  -thread \
  -linkpkg \
  -w YSPUZF \
  -warn-error YSPUZ

OCAMLOPT=ocamlfind ocamlopt
OCAMLDEP=ocamlfind ocamldep
OCAMLYACC=ocamlfind ocamlyacc
OCAMLLEX=ocamlfind ocamllex -q

all: init $T
	@echo done

init:
	eval `opam config -env`

OBJECTS=$(addsuffix .cmx, $(MODULES))
SYNTAX_EXT_PACKAGES=$(addsuffix .syntax, $(SYNTAX_EXTS))

$B.cmx: $(OBJECTS)
	$(OCAMLOPT) -pack $(OBJECTS) -o $B.cmx

$B.cmo: $(OBJECTS)
	$(OCAMLC) -pack $(OBJECTS) -o $B.cmo

std.cmx: std.ml
	$(OCAMLOPT) $(OCAMLOPT_FLAGS) $(FOR_PACK_OPT) -c std.ml

%.exe: $(OBJECTS)
	$(OCAMLOPT) -thread $(addprefix -package , $(PACKAGES)) -linkpkg $(OBJECTS) -o $*.exe

%.cma: %.cmo
	$(OCAMLC) -thread -a $*.cmo -o $*.cma

%.cmxa: %.cmx
	$(OCAMLOPT) -a $*.cmx -o $*.cmxa

%_intf.mli: %_intf.ml
	ln -sf $*_intf.ml $*_intf.mli

%.cmi: %.mli
	$(OCAMLOPT) $(OCAMLOPT_FLAGS) -c $*.mli

%.cmx: %.cmi %.ml
	$(OCAMLOPT) $(OCAMLOPT_FLAGS) $(FOR_PACK_OPT) -c $*.ml

%.ml %.mli: %.mly
	$(OCAMLYACC) $*.mly

%.ml: %.mli %.mll
	$(OCAMLLEX) $*.mll

.depend:
	$(OCAMLDEP) $(FIND_OPTS) *.mli *.ml > .depend

clean:
	rm -rf .depend *.o *.a *.cmi *.cmo *.cma *.cmx *.cmxa *.exe

include .depend

