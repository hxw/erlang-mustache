# Makefile

PREFIX ?= ../
DEST := $(PREFIX)$(PROJECT)

REBAR ?= ./rebar

all:
	@$(REBAR) get-deps compile

update:
	@for d in deps/*; \
	  do (cd "$${d}" && git checkout --force master && git pull); \
	done
	@$(REBAR) update-dep
	@for d in deps/*; \
	do \
	  echo $${d}; \
	  (cd "$${d}" && git checkout --force master); \
        done
	@for dir in deps/*; \
	do \
	  base_dir=$$(basename "$${dir}"); \
	  for patchfile in patches/patch-$${base_dir}_*; \
	  do \
	    [ -e "$${patchfile}" ] || continue; \
	    echo applying patch: $${patchfile}; \
	    pf=$$(realpath "$${patchfile}"); \
	    (cd "$${dir}" && patch -p1 < "$${pf}"); \
	  done; \
	done

edoc:
	@$(REBAR) doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean

build_plt:
#	@$(REBAR) build-plt
	dialyzer --build_plt --apps kernel stdlib crypto compiler xmerl mnesia inets  # ssl

# modules which dialyzer cannot handle
IGNORE = deps/mongodb/%
# dialyzer dependencies
D_INC = $(foreach dir,$(filter-out ${IGNORE},$(wildcard deps/*/include)),-I ${dir})
D_SRC = $(foreach dir,$(filter-out ${IGNORE},$(wildcard deps/*/src)),-c ${dir})
DIALYZER_DEPS = ${D_INC} ${D_SRC}
dialyzer:
#	@$(REBAR) dialyse
	dialyzer -I deps -c src ${DIALYZER_DEPS} --src --verbose

app:
	@$(REBAR) create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)
