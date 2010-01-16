#
# This file is part of Kaputt.
# Copyright (C) 2008-2009 Xavier Clerc.
#
# Kaputt is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# Kaputt is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# PATHS

PATH_BASE=$(shell pwd)
PATH_SRC=$(PATH_BASE)/src
PATH_BIN=$(PATH_BASE)/bin
PATH_DOC=$(PATH_BASE)/ocamldoc
PATH_TESTS=$(PATH_BASE)/tests
PATH_OCAML_BIN=$(shell which ocamlc > /dev/null && dirname `which ocamlc` || echo '')
ifeq ($(PATH_OCAML_BIN),)
$(error cannot find path of OCaml compilers)
endif


# DEFINITIONS

OCAMLC=$(PATH_OCAML_BIN)/ocamlc
OCAMLOPT=$(PATH_OCAML_BIN)/ocamlopt
OCAMLJAVA=$(PATH_OCAML_BIN)/ocamljava
ifeq ($(findstring $(OCAMLJAVA),$(wildcard $(OCAMLJAVA))),$(OCAMLJAVA))
OCAMLJAVA_AVAILABLE=yes
else
OCAMLJAVA_AVAILABLE=no
endif
OCAMLDOC=$(PATH_OCAML_BIN)/ocamldoc
OCAML_COMPILE_FLAGS=-w Ael -warn-error A -I $(PATH_SRC) -for-pack Kaputt
OCAML_JAVA_FLAGS=-java-package fr.x9c.kaputt
OCAML_LIBRARIES=

LIBRARY=kaputt
OCAML_DOC_TITLE=Kaputt $(shell cat VERSION)

INSTALL_DIR_BASE=$(shell $(OCAMLC) -where)
INSTALL_DIR=$(INSTALL_DIR_BASE)/kaputt

CMA_FILES=$(patsubst %,%.cma,$(OCAML_LIBRARIES))
CMXA_FILES=$(patsubst %,%.cmxa,$(OCAML_LIBRARIES))
CMJA_FILES=$(patsubst %,%.cmja,$(OCAML_LIBRARIES))

MODULES=utils assertion generator enumerator specification shell test abbreviations
NOT_PACKED_MODULES=kaputtBigarray kaputtNums

ifeq ($(OCAMLJAVA_AVAILABLE),yes)
	EXTENSIONS=cmi cmo cmx cmj
else
	EXTENSIONS=cmi cmo cmx
endif

CMI_FILES=$(patsubst %,$(PATH_SRC)/%.cmi,$(MODULES))
CMO_FILES=$(patsubst %,$(PATH_SRC)/%.cmo,$(MODULES))
CMX_FILES=$(patsubst %,$(PATH_SRC)/%.cmx,$(MODULES))
CMJ_FILES=$(patsubst %,$(PATH_SRC)/%.cmj,$(MODULES))


# TARGETS

default:
	@echo "available targets:"
	@echo "  all         compiles all files, and generates html documentation"
	@echo "  bytecode    compiles the bytecode version (ocamlc)"
	@echo "  native      compiles the native version (ocamlopt)"
	@echo "  java        compiles the java version (ocamljava)"
	@echo "  html-doc    generates html documentation"
	@echo "  clean-all   deletes all produced files (including documentation)"
	@echo "  clean       deletes all produced files (excluding documentation)"
	@echo "  clean-doc   deletes documentation files"
	@echo "  install     copies library files"
	@echo "  ocamlfind   installs through ocamlfind"
	@echo "  tests       runs the tests"
	@echo "  depend      populates the dependency files (they are initially empty)"
	@echo "installation is usually done by: 'make all' and 'sudo make install'"


ifeq ($(OCAMLJAVA_AVAILABLE),yes)
all: clean-all bytecode native java html-doc
else
all: clean-all bytecode native html-doc
endif

bytecode: $(CMI_FILES) $(CMO_FILES)
	$(OCAMLC) -I $(PATH_SRC) -pack -o $(LIBRARY).cmo $(CMO_FILES)
	$(OCAMLC) -a -o $(LIBRARY).cma $(LIBRARY).cmo
	mv $(LIBRARY).cm* $(PATH_BIN)
	for file in $(patsubst %,$(PATH_SRC)/%.mli,$(NOT_PACKED_MODULES)); do \
	  $(OCAMLC) -c -w Ael -warn-error A -I $(PATH_SRC) -I $(PATH_BIN) $$file; \
	done
	for file in $(patsubst %,$(PATH_SRC)/%.ml,$(NOT_PACKED_MODULES)); do \
	  $(OCAMLC) -c -w Ael -warn-error A -I $(PATH_SRC) -I $(PATH_BIN) $$file; \
	done
	cp $(patsubst %,$(PATH_SRC)/%.cmi,$(NOT_PACKED_MODULES)) $(PATH_BIN)
	cp $(patsubst %,$(PATH_SRC)/%.cmo,$(NOT_PACKED_MODULES)) $(PATH_BIN)

native: $(CMI_FILES) $(CMX_FILES)
	$(OCAMLOPT) -I $(PATH_SRC) -pack -o $(LIBRARY).cmx $(CMX_FILES)
	$(OCAMLOPT) -a -o $(LIBRARY).cmxa $(LIBRARY).cmx
	mv $(LIBRARY).cm* $(LIBRARY).a $(PATH_BIN)
	rm $(LIBRARY).o
	for file in $(patsubst %,$(PATH_SRC)/%.ml,$(NOT_PACKED_MODULES)); do \
	  $(OCAMLOPT) -c -w Ael -warn-error A -I $(PATH_SRC) -I $(PATH_BIN) $$file; \
	done
	cp $(patsubst %,$(PATH_SRC)/%.cmx,$(NOT_PACKED_MODULES)) $(PATH_BIN)
	cp $(patsubst %,$(PATH_SRC)/%.o,$(NOT_PACKED_MODULES)) $(PATH_BIN)

java: $(CMI_FILES) $(CMJ_FILES)
	$(OCAMLJAVA) -I $(PATH_SRC) -pack -o $(LIBRARY).cmj $(CMJ_FILES)
	$(OCAMLJAVA) -a -o $(LIBRARY).cmja $(LIBRARY).cmj
	mv $(LIBRARY).cm* $(LIBRARY).jar $(PATH_BIN)
	rm $(LIBRARY).jo
	for file in $(patsubst %,$(PATH_SRC)/%.ml,$(NOT_PACKED_MODULES)); do \
	  $(OCAMLJAVA) -c -w Ael -warn-error A -I $(PATH_SRC) -I $(PATH_BIN) $(OCAML_JAVA_FLAGS) $$file; \
	done
	cp $(patsubst %,$(PATH_SRC)/%.cmj,$(NOT_PACKED_MODULES)) $(PATH_BIN)
	cp $(patsubst %,$(PATH_SRC)/%.jo,$(NOT_PACKED_MODULES)) $(PATH_BIN)

html-doc:
	$(OCAMLDOC) -sort -html -t '$(OCAML_DOC_TITLE)' -d $(PATH_DOC) -I $(PATH_SRC) -I $(PATH_BIN) $(PATH_SRC)/*.mli

clean-all: clean clean-doc

clean:
	rm -f $(PATH_SRC)/*.cm*
	rm -f $(PATH_SRC)/*.o
	rm -f $(PATH_SRC)/*.jo
	rm -f $(PATH_BIN)/*.*

clean-doc:
	rm -f $(PATH_DOC)/*.html
	rm -f $(PATH_DOC)/*.css

install:
	mkdir -p $(INSTALL_DIR)
	cp $(PATH_BIN)/$(LIBRARY)*.* $(INSTALL_DIR)

ocamlfind:
	ocamlfind query kaputt && ocamlfind remove kaputt || echo ''
	ocamlfind install kaputt META $(PATH_BIN)/$(LIBRARY)*.*

tests::
	@for id in tests/*; do \
	  if [ -d $$id ]; then \
	    echo " *** running '$$id' tests (bytecode)"; \
	    $(MAKE) -s -C $$id PATH_OCAML_BIN=$(PATH_OCAML_BIN) COMPILER=ocamlc EXECUTABLE=bytecode RUN=./ LIB_EXT=cma; \
	    echo " *** running '$$id' tests (native)"; \
	    $(MAKE) -s -C $$id PATH_OCAML_BIN=$(PATH_OCAML_BIN) COMPILER=ocamlopt EXECUTABLE=native RUN=./ LIB_EXT=cmxa; \
	    if [ $(OCAMLJAVA_AVAILABLE) == yes ]; then \
	      echo " *** running '$$id' tests (java)"; \
	      $(MAKE) -s -C $$id PATH_OCAML_BIN=$(PATH_OCAML_BIN) COMPILER=ocamljava FLAGS=-standalone EXECUTABLE=prog.jar RUN='java -jar ' LIB_EXT=cmja; \
	    fi \
	  fi \
	done


# GENERIC TARGETS

.SUFFIXES: .ml .mli .cmo .cmi .cmx .cmj

.mli.cmi:
	$(OCAMLC) $(OCAML_COMPILE_FLAGS) -c $<

.ml.cmo:
	$(OCAMLC) $(OCAML_COMPILE_FLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAML_COMPILE_FLAGS) -c $<

.ml.cmj:
	$(OCAMLJAVA) $(OCAML_JAVA_FLAGS) $(OCAML_COMPILE_FLAGS) -c $<


# DEPENDENCIES

depend::
	$(OCAMLDEP) -I $(PATH_SRC) $(PATH_SRC)/*.ml* > depend
	$(OCAMLDEP) -I $(PATH_SRC) $(PATH_SRC)/*.ml* | sed 's/\.cmx/\.cmj/g'> depend.cafesterol

include depend
include depend.cafesterol
