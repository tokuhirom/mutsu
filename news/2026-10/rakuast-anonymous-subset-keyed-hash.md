RakuAST now preserves anonymous subset types used as keyed-hash keys. The
structured subset expression remains available in the RakuAST tree, while
lowering retains the original key constraint for runtime binding.
