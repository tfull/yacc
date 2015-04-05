CC = gcc

bin/Main: bin/Tokenizer.o bin/Parser.o src/Main.c
	$(CC) -o $@ $^

bin/Parser.o: src/Parser.c
	$(CC) -c -O2 -o $@ $^

bin/Tokenizer.o: src/Tokenizer.c
	$(CC) -c -Wall -O2 -o $@ $^

clean:
	rm bin/*
