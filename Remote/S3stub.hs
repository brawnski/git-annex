-- stub for when hS3 is not available
module Remote.S3 (remote) where

import RemoteClass
import Types

remote :: RemoteType Annex
remote = RemoteType {
	typename = "S3",
	enumerate = return [],
	generate = error "S3 not enabled",
	setup = error "S3 not enabled"
}
