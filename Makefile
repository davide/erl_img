
code: clean
	erl -noshell -pa ebin -s make all -s erlang halt

run:	code
	werl -pa ebin &
	
clean:
	rm -fv ebin/*.beam erl_crash.dump
