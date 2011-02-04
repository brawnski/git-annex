{- a simple graphviz / dot(1) digraph description generator library
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Dot where -- import qualified

{- generates a graph description from a list of lines -}
graph :: [String] -> String
graph s = unlines $ [header] ++ s ++ [footer]
	where
		header = "digraph map {"
		footer= "}"

{- a node in the graph -}
graphNode :: String -> String -> String
graphNode nodeid desc = lineLabeled desc $ quote nodeid

{- an edge between two nodes -}
graphEdge :: String -> String -> Maybe String -> String
graphEdge fromid toid d =
	case d of
		Nothing -> line edge
		Just desc -> lineLabeled desc edge
	where
		edge = quote fromid ++ " -> " ++ quote toid

quote :: String -> String
quote s = "\"" ++ s ++ "\""

line :: String -> String
line s = "\t" ++ s ++ ";"

{- a line with a label -}
lineLabeled :: String -> String -> String
lineLabeled label s = line $ s ++ " [ label=" ++ quote label ++ " ]"

{- apply to graphNode to put the node in a labeled box -}
subGraph :: String -> String -> String -> String
subGraph subid label s = line $
	"subgraph " ++ name ++ "{\n" ++ setlabel ++ "\n" ++ s ++ "\n}"
	where
		-- the "cluster_" makes dot draw a box
		name = quote ("cluster_" ++ subid)
		setlabel = line $ "label=" ++ quote label
