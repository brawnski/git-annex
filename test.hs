import Test.HUnit
import Test.HUnit.Tools

import GitRepo
import Locations

alltests :: [Test]
alltests = [
	qctest "prop_idempotent_deencode" prop_idempotent_deencode,
	qctest "prop_idempotent_fileKey" prop_idempotent_fileKey
	]

main :: IO (Counts, Int)
main = runVerboseTests (TestList alltests)
