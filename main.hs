
import Parse
import Lex
import qualified Verify
import Expression

succeedParse tree = do
	case Verify.verifyAll (Right tree) of
		Verify.Success -> putStrLn "Program is correct."
		Verify.Failure messages -> mapM_ describeFailure messages

describeFailure (t, message) = do
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
	let exampleLedex = lexer example "example.ni"
	let exampleParsed = run parseStatement ("example.ni", exampleLedex)
	case exampleParsed of
		Success tree [] -> succeedParse tree
		Error message rest location -> failParse message rest location