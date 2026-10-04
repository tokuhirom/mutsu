use v6;
use Test;

plan 11;

# Every `grep` candidate takes a matcher, so `.grep()` with none -- with or
# without an adverb -- resolves no candidate and dies X::Multi::NoMatch, as in
# rakudo. It used to return the invocant's elements unfiltered (#11630).

throws-like { (1, 2, 3).grep() }, X::Multi::NoMatch,
    message => /'Cannot resolve caller grep(List:D: )'/, 'List.grep()';
throws-like { [1, 2].grep() }, X::Multi::NoMatch,
    message => /'grep(Array:D: )'/, 'Array.grep()';
throws-like { (1..3).grep() }, X::Multi::NoMatch,
    message => /'grep(Range:D: )'/, 'Range.grep()';
throws-like { (1, 2).Seq.grep() }, X::Multi::NoMatch, 'Seq.grep()';
throws-like { %(a => 1).grep() }, X::Multi::NoMatch, 'Hash.grep()';
throws-like { (1, 2, 3).grep(:k) }, X::Multi::NoMatch,
    message => /'grep(List:D: :k)'/, 'an adverb is not a matcher';
throws-like { (1, 2, 3).grep() }, X::Multi::NoMatch,
    message => /'($:: Mu $t, *%_)'/, 'the message lists the candidates';

# With a matcher nothing changes.
is-deeply (1, 2, 3).grep(* > 1).List, (2, 3), 'a WhateverCode matcher';
is-deeply (1, 2, 3).grep(2, :k).List, (1,), 'a matcher with :k';
is-deeply (1, 2, 3).grep(Int, :p).List, (0 => 1, 1 => 2, 2 => 3), 'a type matcher with :p';
is-deeply grep((1, 2)).List, (), 'the routine form with only a matcher';
