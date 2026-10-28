# A multi candidate is ranked with its parameter types resolved in its own scope

`my constant StrType = Str but Type;` in a module, used as a `multi` parameter
type (`multi kind(StrType:D $_)`), is invisible to a caller that imports only
the routine. The candidate was ranked in the caller's scope, where `StrType`
names nothing, so it lost to the plain `Str:D` candidate and Needle::Compile's
`implicit2explicit` answered the wrong form. Ranking (`candidate_rank_key`) now
runs in the candidate's declaring package and compunit, as its `where` clauses
and parameter matching already did. With the load failure of #15164 worked
around locally, all four `Needle::Compile` test files pass. Fixes #15087.
