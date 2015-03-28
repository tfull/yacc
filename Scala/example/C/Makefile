bin/Main: bin/Tokenizer.o src/Main.c
	gcc -o $@ $^

bin/Tokenizer.o: src/Tokenizer.c
	gcc -c -o $@ $^

clean:
	rm bin/*
