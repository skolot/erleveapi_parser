DEPS = $(wildcard deps/*)
APPS = .
SUBDIRS := $(DEPS) $(APPS) test

MKINCDIR ?= $(PWD)

indir = .
outdir = .

export VERBOSE = @
export DEVNULL = >/dev/null
SILENT = -s

ifeq "$(V)" "1"
VERBOSE =
DEVNULL =
SILENT = 
endif

all: compile 

clean compile:
	$(VERBOSE)cd $(indir) && \
	for d in $(SUBDIRS); do \
		$(MAKE) $(SILENT) -C $(indir)/$${d} -f $(MKINCDIR)/erlang.mk indir=$(indir)/$${d} outdir=$(outdir)/$${d} subdir=$${d} $@ || break; \
	done

test: compile
	$(VERBOSE)erl -noshell -pa ebin -eval 'eunit:test(eveapi_parser_SUITE)' -s init stop
