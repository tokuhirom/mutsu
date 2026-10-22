RakuAST now retains expression-valued regex adverbs and Unicode property
predicates through conversion and lowering. This covers counted modifiers with
lists and Whatever-derived indexes, numeric Unicode properties, and regex-valued
name predicates, using the existing regex parser and matcher.

The focused RakuAST regression has 13 assertions. Three previously refused
roast files now pass in the RakuAST frontend (160 assertions).
