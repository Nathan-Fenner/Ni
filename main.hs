
import Parse
import Lex
import qualified Verify
import Expression
import CompileC
import System.Environment(getArgs)
import System.Exit

succeedParse :: Statement -> IO ()
succeedParse tree = do
	case Verify.verifyProgram tree of
		Verify.Pass () -> do
			putStrLn "Program is correct."
			writeFile "out.c" $ compileProgram tree
		Verify.Flunk messages -> mapM_ describeFailure messages

describeFailure :: Verify.Message -> IO ()
describeFailure (Verify.Message t message) = do
	putStrLn $ "error: " ++ message
	putStrLn $ "at " ++ pretty (at t) ++ " (token `" ++ token t ++ "`)"

failParse :: String -> [Token] -> Location -> IO ()
failParse message rest location = do
	putStrLn "Error while parsing:"
	putStrLn message
	putStrLn $ "at " ++ pretty location
	putStrLn $ "The following tokens are " ++ show (take 3 $ rest)
	return ()

main :: IO ()
main = do
	args <- getArgs
	case args of
		[] -> putStrLn "usage:\n\tmain [file.ni]"
		(_:_:_) -> putStrLn "usage:\n\tmain [file.ni]"
		[fname] -> do
			example <- readFile fname
			let exampleLexed = lexer example fname
			let exampleParsed = run parseModule (fname, exampleLexed)
			case exampleParsed of
				Success tree [] -> succeedParse tree >> exitSuccess
				Success _tree rest -> (putStrLn $ "didn't consume all input: " ++ show rest) >> exitWith (ExitFailure 1)
				Error _ message rest location -> failParse message rest location >> exitWith (ExitFailure 2)
