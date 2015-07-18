
import Parse
import Lex
import qualified Verify
import Expression
import Compile

succeedParse tree = do
	case Verify.verifyProgram tree of
		Verify.Pass () -> do
			putStrLn "Program is correct."
			writeFile "out.js" $ compileProgram tree
		Verify.Flunk messages -> mapM_ describeFailure messages

describeFailure (Verify.Message t message) = do
	putStrLn $ "error: " ++ message
	putStrLn $ "at " ++ pretty (at t) ++ " (token `" ++ token t ++ "`)"

failParse message rest location = do
	putStrLn "Error while parsing:"
	putStrLn message
	putStrLn $ "at " ++ pretty location
	putStrLn $ "The following tokens are " ++ show (take 3 $ rest)
	return ()

main = do
	example <- readFile "example.ni"
	let exampleLexed = lexer example "example.ni"
	let exampleParsed = run parseStatement ("example.ni", exampleLexed)
	case exampleParsed of
		Success tree [] -> succeedParse tree
		Error message rest location -> failParse message rest location