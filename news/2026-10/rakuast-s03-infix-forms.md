RakuAST now round-trips negated infixes, declared symbolic infixes, and nested
bracketed compound assignment, including bracketed reverse, cross and zip
metaoperators. Read-only setting-term assignments retain their
source precedence and throw before an enclosing call runs.
