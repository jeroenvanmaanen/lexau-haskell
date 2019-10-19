.PHONY: default clean install-lib install-deps setup

default: all
all: build

include config.mk

# If not set in custom config.mk, use the default versions
HC      ?= ghc
PKG     ?= ghc-pkg
HADDOCK ?= haddock

DIST = dist
DIST_LIB  = $(DIST)/lib
DIST_SERVER = $(DIST)/server
SETUP_DIST = setup-dist
SETUP = $(SETUP_DIST)/Setup
GENERATED = src/LExAu/IO/Words.hs src/Data/PrettyPrint/Parens.hs

DOTDOTSETUP = cabal

CABAL_INSTALL_OPTS += --ghc --with-compiler=$(HC) --with-hc-pkg=$(PKG)
CABAL_FLAGS ?= 
# -ftesting

$(DIST)/setup-config: $(SETUP) lexau-totem.cabal $(DIST)
	$(SETUP) configure -v --builddir=$(DIST) \
	     --with-compiler=$(HC) --with-hc-pkg=$(PKG) \
             --user $(CABAL_FLAGS) > $(DIST)/lib-config-log

src/LExAu/IO/Words.hs: src/LExAu/IO/Words.x
	@echo === Generating Words lexer ===
	alex -g $< -o $@

src/Data/PrettyPrint/Parens.hs: src/Data/PrettyPrint/Parens.x
	@echo === Generating Parens lexer ===
	alex -g $< -o $@

$(DIST)/build/lexau/lexau: $(SETUP) $(DIST)/setup-config $(wildcard src/*.hs src/**/*.hs src/**/**/*.hs src/**/**/**/*.hs) $(GENERATED)
	@echo === Building lexau ===
	$(SETUP) build --builddir=$(DIST)

$(DIST):
	mkdir $(DIST)

$(SETUP): Setup.hs $(SETUP_DIST)
	$(HC) --make $< -o $@
	rm -rf Setup.hi Setup.o

$(SETUP_DIST):
	mkdir $@

setup: $(SETUP)

build: $(DIST)/build/lexau/lexau

# TODO: dodgy
install: $(DIST)/build/lexau/lexau
	cabal install

# test: build
# 	echo main | $(HC) --interactive -package ghc -DDEBUG -isrc -idist/build tests/RunTests.hs
# #	./dist/build/test_get_imports/test_get_imports $(GHC_PATH)/compiler dist-stage2 +RTS -s -RTS

clean:
	$(SETUP) clean --builddir=$(DIST) || rm -rf $(DIST)

distclean: clean
	rm -rf $(SETUP_DIST)

doc: setup
	$(SETUP) haddock --with-haddock=$(HADDOCK) --executables

printvars:
	@echo "UseInplaceGhc    = $(UseInplaceGhc)"
	@echo "GHC_PATH         = $(GHC_PATH)"
	@echo "HC               = $(HC)"
	@echo "PKG              = $(PKG)"
	@echo "HADDOCK          = $(HADDOCK)"
	@echo "CABAL_INSTALL    = $(CABAL_INSTALL)"
	@echo "        ..._OPTS = $(CABAL_INSTALL_OPTS)"
	@echo "CABAL_FLAGS      = $(CABAL_FLAGS)"
	@echo "---------------------------------------------------------------"
	@echo "DIST_LIB     = $(DIST_LIB)"
	@echo "SETUP_DIST   = $(SETUP_DIST)"
