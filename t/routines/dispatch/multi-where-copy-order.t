use v6;
use Test;

# `is copy` changes parameter binding, not dispatch specificity. The first
# candidate must therefore win when it and a later `is copy where` candidate
# have the same nominal type and both constraints match.
plan 3;

proto sub dispatch-copy-where(Int $n) {*}
multi sub dispatch-copy-where(Int $n where $n <= 1) { 'base' }
multi sub dispatch-copy-where(Int $n where !($n %% 2)) { 'odd' }
multi sub dispatch-copy-where(Int $n is copy where $n %% 2) { 'even-copy' }

is dispatch-copy-where(0), 'base', 'earlier where candidate beats later is copy candidate';
is dispatch-copy-where(2), 'even-copy', 'is copy candidate still handles its matching values';
is dispatch-copy-where(3), 'odd', 'the other where candidate still handles odd values';

done-testing;
