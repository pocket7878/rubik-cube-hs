OUT=rubik
HSS=Main.hs Perm.hs Rubik.hs
$(OUT): $(HSS)
	ghc -o $@ $^
run: $(OUT)
	@./$(OUT)
clean: 
	rm *.hi *.o $(OUT)
doc: Perm.hs Rubik.hs
	haddock -h -o doc/ Perm.hs
	haddock -h -o doc/ Rubik.hs

