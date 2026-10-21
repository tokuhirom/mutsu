RakuAST now round-trips seven more declaration and source-boundary test files:
DOC phasers, role augmentation, legacy alternate signatures and `supersede`
routines, Whatever composition, and heredocs that close their enclosing block.
The public constructors keep Rakudo's visible shape while private source
provenance preserves the parser's executable behavior.
