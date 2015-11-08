compile:
	../rebar3 compile
release:
	../rebar3 release

clean:
	../rebar3 clean

console: clean compile release
	./_build/default/rel/crawler/bin/crawler console

run:
	./_build/default/rel/crawler/bin/crawler foreground
