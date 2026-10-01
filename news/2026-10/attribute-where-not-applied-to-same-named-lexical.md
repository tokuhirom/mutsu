# Attribute `where` clauses no longer constrain same-named lexicals

A scalar assignment inside a method's call stack looked up the assigned variable's bare name
among the invocant's attributes, so `my $day = ...` in a helper sub called from a method of a
class with `has Int $.day where {...}` was rejected with `expected <anon>`. Only twigil names
(`$!day`, `$.day`) now resolve to attributes. Found via Date::Calendar::Hebrew
(`t/02-accessors.rakutest` now passes 25/25).
