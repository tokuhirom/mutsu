Role declarations carrying `:ver`, `:auth` or `:api` now round-trip through
RakuAST. The shared package-header bridge records these as colonpairs on the
role name, retains a following parameterization signature, and restores the
parser's metadata setters when lowering. A role body lexical remains visible to
its methods after the round trip.
