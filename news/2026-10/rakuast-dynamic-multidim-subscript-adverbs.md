RakuAST now preserves dynamic `:$delete` adverbs on 6.e multidimensional
array and hash subscripts. Bare deletion, value adverbs, and existence adverbs
round-trip as postcircumfix colonpairs; the array and hash multislice roast
files pass in RakuAST frontend mode.
