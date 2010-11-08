-- TODO find a test harness that is actually in Debian and use it.

import Test.QuickCheck
import Test.HUnit
import Test.HUnit.Tools

import GitRepo
import Locations

alltests = [
	qctest "prop_idempotent_deencode" prop_idempotent_deencode,
	qctest "prop_idempotent_fileKey" prop_idempotent_fileKey
	]

main = runVerboseTests (TestList alltests)
