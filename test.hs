import Test.HUnit
import Test.HUnit.Tools

import GitRepo
import Locations
import Utility
import TypeInternals

alltests :: [Test]
alltests = 
	[ qctest "prop_idempotent_deencode" prop_idempotent_deencode
	, qctest "prop_idempotent_fileKey" prop_idempotent_fileKey
	, qctest "prop_idempotent_key_read_show" prop_idempotent_key_read_show
	, qctest "prop_idempotent_shellEscape" prop_idempotent_shellEscape
	, qctest "prop_idempotent_shellEscape_multiword" prop_idempotent_shellEscape_multiword
	, qctest "prop_parentDir_basics" prop_parentDir_basics
	, qctest "prop_relPathDirToDir_basics" prop_relPathDirToDir_basics
	]

main :: IO (Counts, Int)
main = runVerboseTests (TestList alltests)
