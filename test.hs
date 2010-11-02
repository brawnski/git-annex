-- TODO find a test harness that is actually in Debian and use it.

import Test.QuickCheck
import Test.HUnit
import Test.HUnit.Tools

import GitRepo

alltests = [
	qctest "prop_idempotent_deencode" prop_idempotent_deencode
	]

main = runVerboseTests (TestList alltests)
