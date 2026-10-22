RakuAST now round-trips negated infixes, declared symbolic infixes, and nested
bracketed compound assignment. Read-only setting-term assignments retain their
source precedence and throw before an enclosing call runs.
