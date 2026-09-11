# Block quantifier bounds survive backtracking

Block quantifiers inside captured and separated regex repetitions now retain
their minimum, maximum, and exact-count bounds when the matcher retries a
candidate.

Pinned by `t/regex/syntax/regex-block-quantifier-backtracking.t`.

Closes #7906.
