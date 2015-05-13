ERLC ?= erlc
ERL ?= erl
INSTALL ?= install
INSTALL_FLAGS ?= -D -m 0644

extra_erlcflags = $($(appname)_ERLCFLAGS)

libdir ?= ../../deps
ebindir = ./ebin
srcdir = ./src
testdir = ./test

parse_transform_modules = $(foreach erl,$(wildcard $(srcdir)/*erl), $(shell grep -h "compile.*parse_transform" $(erl) | sed -e 's@-compile({parse_transform,\(.*\)}).@\1@' | sort -u | while read mod; do test -f $(srcdir)/$${mod}.erl && echo $${mod}; done))

erl_modules = $(foreach erl,$(wildcard $(srcdir)/*erl), $(notdir $(basename $(shell grep -q "compile.*parse_transform" $(erl) || echo $(erl)))))

erl_src_files = $(call parse_transform_modules) $(call erl_modules)
beam_out_files = $(addprefix $(ebindir)/,$(addsuffix .beam, $(erl_src_files)))

erl_test_src_files = $(notdir $(basename $(wildcard $(testdir)/*_SUITE.erl)))
beam_test_out_files = $(addprefix $(ebindir)/,$(addsuffix .beam, $(erl_test_src_files)))

app_src_files = $(notdir $(basename $(wildcard $(srcdir)/*app.src)))
app_out_files = $(addprefix $(ebindir)/,$(app_src_files))

PATHA ?= $(addprefix -pa ,$(wildcard $(libdir)/*/ebin))
ERLCFLAGS = -I. -I.. -I../../deps -Iinclude $(extra_erlcflags)

$(ebindir)/%.beam: $(srcdir)/%.erl
	$(VERBOSE)echo "{$(appname)}[beam]" $(subdir)/$@ $(extra_erlcflags)
	$(VERBOSE)$(ERLC) $(ERLCFLAGS) -o $(ebindir) $^

$(ebindir)/%.beam: $(testdir)/%.erl
	$(VERBOSE)echo "{$(appname)}[test_suit]" $(subdir)/$@ $(extra_erlcflags)
	$(VERBOSE)$(ERLC) $(ERLCFLAGS) -o $(ebindir) $^

$(ebindir)/%.app: $(srcdir)/%.app.src
	$(VERBOSE)echo "{$(appname)}[app]" $(subdir)/$@
	$(VERBOSE)erl -noshell \
		-eval 'case file:consult("$<") of {ok,_} -> ok ; \
		{error,{L,M,T}} -> io:format("$<: ~s ~s ~s ~n", [L,M,T]), halt(1) end.' \
		-s init stop
	$(VERBOSE)cp $< $@

mkdir = $(ebindir)

prepare: $(mkdir)

$(mkdir):
	$(VERBOSE)echo "[mkdir]" $@
	$(VERBOSE)mkdir -p $@

compile: prepare $(beam_out_files) $(app_out_files) $(etc_out_files) $(priv_out_files) $(beam_test_out_files)

clean_files = $(beam_out_files)	$(app_out_files) $(etc_out_files) $(beam_test_out_files)

clean:
ifneq "$(clean_files)" ""
	$(VERBOSE)rm -f $(clean_files)
endif

.PHONY: clean build prepare
