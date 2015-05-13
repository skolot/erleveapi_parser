MKINCDIR ?= $(PWD)

SHELL = bash
GIT ?= git
ECHO ?= echo

export VERBOSE = @
export DEVNULL = >/dev/null
export GITSUPPRES = -q
SILENT = -s

ifeq "$(V)" "1"
VERBOSE =
DEVNULL =
SILENT =
GITSUPPRES =
endif

DEPSDST ?= deps

ifneq "$(realpath config.mk)" ""
include config.mk
else
$(error "config.mk doesn't exist!")
endif

APPS = $(DEPS) $(APP)

export $(addsuffix _ERLCFLAGS, $(APPS))
export $(addsuffix _DIR, $(APPS))

GET_DEPS = $(addprefix get-deps-, $(DEPS))
UPDATE_DEPS = $(addprefix update-deps-, $(DEPS))
COMPILE_APPS = $(addprefix compile-, $(APPS))
CLEAN_APPS =$(addprefix clean-, $(APPS))

all: prepare get-deps update-deps compile

prepare: mkdir

mkdir: $(DEPSDST)

$(DEPSDST):
	$(VERBOSE)$(ECHO) "[mkdir] $@"
	$(VERBOSE)mkdir -p $@

$(GET_DEPS):
	$(VERBOSE)$(ECHO) "[get dep] $(subst get-deps-,,$@)"; \
		[ ! -r $($(subst get-deps-,,$@)_DIR)/.git ] && \
		$(GIT) clone $(GITSUPPRES) $($(subst get-deps-,,$@)_SOURCE) $($(subst get-deps-,,$@)_DIR); exit 0

get-deps: $(GET_DEPS)

$(UPDATE_DEPS):
	$(VERBOSE)$(ECHO) "[update dep] $(subst update-deps-,,$@)"
	$(VERBOSE)[ -r $($(subst update-deps-,,$@)_DIR)/.git ] && \
		cd $($(subst update-deps-,,$@)_DIR) && $(GIT) pull $(GITSUPPRES) || \
		$(ECHO) "$(subst update-deps-,,$@) doesn't exist, please run \`make get-deps\'"

update-deps: $(UPDATE_DEPS)

compile: $(COMPILE_APPS)

$(COMPILE_APPS):
	$(VERBOSE)$(MAKE) $(SILENT) -C $($(subst compile-,,$(@))_DIR) -f $(MKINCDIR)/build.mk appname=$(subst compile-,,$(@)) compile

clean: $(CLEAN_APPS)

$(CLEAN_APPS):
	$(VERBOSE)$(MAKE) $(SILENT) -C $($(subst clean-,,$(@))_DIR) -f $(MKINCDIR)/build.mk appname=$(subst clean-,,$(@)) clean


test: compile
	$(VERBOSE)erl -noshell -pa ebin -eval 'eunit:test(eveapi_parser_SUITE)' -s init stop

.PHONY: get-deps update-deps prepare mkdir clean compile test $(GET_DEPS) $(UPDATE_DEPS) $(COMPILE_APPS) $(CLEAN_APPS)
