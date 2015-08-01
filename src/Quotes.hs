
module Quotes where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

lit :: QuasiQuoter
lit = QuasiQuoter 
	{ quoteExp = TH.litE . TH.StringL . process
	, quotePat = error "the `lit` QuasiQuoter is defined only for expressions"
	, quoteType = error "the `lit` QuasiQuoter is defined only for expressions"
	, quoteDec = error "the `lit` QuasiQuoter is defined only for expressions"
	}

process :: String -> String
process ('\n' : rest) = rest
process s = s
