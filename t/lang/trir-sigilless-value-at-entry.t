use Test;
use nqp;

plan 8;

# A sigilless parameter binds the caller's CONTAINER when handed a variable.
# The typed tier (TRIR) holds values, so it may bind the argument's value at
# entry only when every read of the parameter runs before anything that could
# change the caller's variable. JSON::Fast's `str-escape(\text)` is that shape:
# it reads `text` once, first, and is called with `$_`. Each routine is called
# repeatedly so the cached (TRIR) call path is the one exercised.

# The str-escape shape: `.NFD` on the parameter, a postfix `$i++`, a `.chr`.
sub esc(\text) {
    my $codes := text.NFD;
    my int $i = -1;
    my int $quotes = 0;
    nqp::while(
      nqp::islt_i(++$i, nqp::elems($codes)),
      nqp::if(nqp::iseq_i(nqp::atpos_i($codes, $i), 34), $quotes++)
    );
    nqp::concat(nqp::strfromcodes($codes), nqp::concat(':', $quotes))
}
is ('a"b"c' for ^5).map({ esc($_) }).join(','), (('a"b"c:2') xx 5).join(','),
    'a sigilless parameter read first takes `$_`';
my $s = 'q"';
is (esc($s) for ^5).join(','), (('q":1') xx 5).join(','),
    'and a plain variable';
is (esc('x' ~ $_) for ^3).join(','), 'x0:0,x1:0,x2:0', 'and a computed value';

# A read AFTER a call sees what the call wrote to the bound variable.
my $g = 'a';
sub bump($by) { $g = $g ~ $by; 0 }
sub after-call(\x) { bump('b'); nqp::concat(x, '!') }
is (after-call($g) for ^3).join(','), 'ab!,abb!,abbb!',
    'a read after a call sees the aliased variable change';

# A read inside a loop that also calls out sees each new value.
my $h = 'a';
sub bump-h($by) { $h = $h ~ $by; 0 }
sub in-loop(\x) {
    my str $acc = '';
    my int $i = 0;
    nqp::while(nqp::islt_i($i, 3), nqp::stmts(($acc = nqp::concat($acc, x)), bump-h('b'), ++$i));
    $acc
}
is in-loop($h), 'aababb', 'a read in a loop sees every write';

# Postfix ++/-- yield the value before the step.
sub post-inc() { my int $i = 5; my int $j = $i++; nqp::add_i(nqp::mul_i($j, 10), $i) }
is (post-inc() for ^3).join(','), '56,56,56', 'postfix ++ yields the old value';
sub post-dec() { my int $i = 5; my int $j = $i--; nqp::add_i(nqp::mul_i($j, 10), $i) }
is (post-dec() for ^3).join(','), '54,54,54', 'postfix -- yields the old value';

# `.chr` / `.ord` on a native value inside the same kind of routine.
sub chr-ord(\text) { my int $o = nqp::ordat(text, 0); nqp::concat(($o + 1).chr, text.ord.Str) }
is (chr-ord($_) for <a b>).join(','), 'b97,c98', '.chr and .ord are value methods';
