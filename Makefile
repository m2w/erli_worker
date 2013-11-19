SHELL=/bin/sh
APP:=erli_api_worker
REBAR:=./rebar
ERL=erl
CT=ct_run
ROOT=`git rev-parse --show-toplevel`
DEV_CONFIG=$(ROOT)/erli_api_worker_dev.config


dep:
	@$(REBAR) get-deps && $(REBAR) update-deps

clean:
	@$(REBAR) clean

distclean: clean
	@$(REBAR) delete-deps

docs:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[]'

test:
	$(CT) -pa $(ROOT)/ebin/ -pa $(ROOT)/deps/*/ebin -dir ct/ -logdir $(ROOT)/ct/logs/ -include $(ROOT)/include/ -cover $(ROOT)/ct/cover.spec -erl_args -config $(DEV_CONFIG)

compile:
	@$(REBAR) compile

all: dep compile

generate: all
	@$(REBAR) generate
