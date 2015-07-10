
import Parse
import Lex
import ParseExpression
import ParseStatement

main = do
	example <- readFile "example.ni"
	let exampleLedex = lexer example "example.ni"
	print exampleLedex
	let exampleParsed = run parseBlock ("example.ni", exampleLedex)
	print exampleParsed
	writeFile "result.txt" $ show exampleParsed