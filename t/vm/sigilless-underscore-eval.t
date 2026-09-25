use Test;

# A sigilless `\_` is distinct from the topical `$_`. EVAL must resolve the
# source spelling back to the sigilless binding without letting its topic save
# and restore overwrite the value.
plan 5;

my \_ = 5;
is _, 5, 'a sigilless underscore reads in the declaring scope';
is EVAL(q[_]), 5, 'EVAL resolves the sigilless underscore';
is EVAL(q[_ + 1]), 6, 'EVAL can use the sigilless underscore in an expression';

given 'topic' {
    is EVAL(q[_]), 5, 'EVAL keeps the sigilless underscore distinct from $_';
    is $_, 'topic', 'EVAL restores the topical $_ unchanged';
}
