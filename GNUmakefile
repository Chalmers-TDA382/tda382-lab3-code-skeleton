all: gui client server cchat

cchat:  cchat.erl server.beam gui.beam 
	erl -compile cchat.erl 

server: server.erl defs.hrl 
	erl -compile server.erl 

client: client.erl defs.hrl lexgrm 
	erl -compile client.erl

gui: gui.erl 
	erl -compile gui.erl

lexgrm : lex.xrl grm.yrl lexgrm.erl
	 erl -compile lexgrm.erl
	 erl -pa ebin -eval "lexgrm:start()" -noshell -detached

clean:
	rm -f *.beam  



