-- TODO find a test harness that is actually in Debian and use it.

import Test.QuickCheck
import GitRepo

main = do
	putStr "prop_idempotent_deencode "
	quickCheck prop_idempotent_deencode
