unit module ReqScopeGrammar;
grammar G { token TOP { a+ } }
our sub parse-it($text) { G.parse($text) }
