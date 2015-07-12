
import Parse
import Lex
import Verify
import Expression

main = do
	example <- readFile "example.ni"
	let exampleLedex = lexer example "example.ni"
	print exampleLedex
	let exampleParsed = run parseStatement ("example.ni", exampleLedex)
	print exampleParsed
	writeFile "result.txt" $ show exampleParsed