bin = bin
compiler = gcc

$(bin)/Main: $(bin)/Tokenizer.o $(bin)/Parser.o src/Main.c
	$(compiler) -o $@ $^

$(bin)/Parser.o: src/Parser.c $(bin)
	$(compiler) -c -O2 -o $@ $<

$(bin)/Tokenizer.o: src/Tokenizer.c $(bin)
	$(compiler) -c -Wall -O2 -o $@ $<

$(bin):
	mkdir $@

clean:
	rm -r $(bin)
