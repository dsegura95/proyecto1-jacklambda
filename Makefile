CC= ghc
Remove.O = *.o
Remove.hi = *.hi

FileLambda: JackLambda.hs
	@echo "Generating Files..."
	$(CC) JackLambda
	@echo "Done."

.PHONY: clean

clean:
	@echo "Cleaning..."
	@rm $(Remove.O) $(Remove.hi)
	@echo "Done."

