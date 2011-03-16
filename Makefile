ERL=erl
APP_NAME=epmail
NODE_NAME=epmail
EFLAGS=-pa ebin -sname $(NODE_NAME)
VSN=0.1

all: compile

compile:
	test -d ebin || mkdir ebin
	$(ERL) $(EFLAGS) -make
	cp $(APP_NAME).app ebin

doc:	
	$(ERL) $(EFLAGS) -noshell \
	-run edoc_run application "'$(APP_NAME)'" '"."' '[{def,{vsn,"$(VSN)"}}]'

clean:
	test ! -d doc || rm -rfv doc
	rm -rfv erl_crash.dump ebin
	find . -name "*~" -exec rm -fv {} \;

# for testing purpose
run: compile
	$(ERL) $(EFLAGS)  -eval 'application:start($(APP_NAME)).'
