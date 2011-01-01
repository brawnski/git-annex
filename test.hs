import Test.HUnit
import Test.HUnit.Tools

import GitRepo
import Locations
import Utility

alltests :: [Test]
alltests = [
	qctest "prop_idempotent_deencode" prop_idempotent_deencode,
	qctest "prop_idempotent_fileKey" prop_idempotent_fileKey,
	qctest "prop_idempotent_shellescape" prop_idempotent_shellescape,
	qctest "prop_idempotent_shellescape_multiword" prop_idempotent_shellescape_multiword
	]

main :: IO (Counts, Int)
main = runVerboseTests (TestList alltests)
