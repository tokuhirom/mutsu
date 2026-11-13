use Test;

# `my \k = EXPR` with a non-variable EXPR binds a value: the term is readonly
# for its whole life. Its readonly state is a fact of its slot (ADR-17376), so
# a routine's own term keeps no env marker -- and every way of writing to it,
# here or through a callee, must still be refused, while reads, aggregates and
# loops behave as before.

plan 24;

sub died(&code) { (try { code(); 'lived' }) // $!.^name }

is died({ sub { my \k = 5; k = 3 }() }), 'X::Assignment::RO', 'assignment';
is died({ sub { my \k = 5; k++ }() }), 'X::Multi::NoMatch', 'postfix ++';
is died({ sub { my \k = 5; ++k }() }), 'X::Multi::NoMatch', 'prefix ++';
is died({ sub { my \k = 5; k += 1 }() }), 'X::Assignment::RO', 'compound assignment';
is died({ sub { my \k = 'a'; k .= uc }() }), 'X::Assignment::RO', '.=';
is died({ sub { my \k = 5; (k, my $y) = 1, 2 }() }), 'X::Assignment::RO', 'list assignment';
is died({ sub { my \k = 5; say(k -= 1) }() }), 'X::Assignment::RO', 'compound assignment as an expression';
is died({ sub { my \k = 5; my \j = k; j = 3 }() }), 'X::Assignment::RO', 'a term bound to the term';

sub raw(\x) { x = 1 }
is died({ sub { my \k = 5; raw(k) }() }), 'X::Assignment::RO', 'a sigilless parameter bound to it';
sub rw($x is rw) { $x = 1 }
is died({ sub { my \k = 5; rw(k) }() }), 'X::Parameter::RW', 'an is rw parameter refuses it';
my &raw-code = &raw;
is died({ sub { my \k = 5; raw-code(k) }() }), 'X::Assignment::RO', 'a call through a code variable';
is died({ sub { my \k = 5; my $p = :k(k); $p.value = 3 }() }), 'X::Assignment::RO', 'a colon pair of it';

is sub { my \k = 5; my $r = (try { raw(k) }); k }(), 5, 'the term keeps its value after a refused write';
is sub { my \k = [1, 2]; k = 3; k.raku }(), '[3]', 'a term bound to an Array assigns into it';
is sub { my \k = [1, 2]; k.push(3); k.elems }(), 3, 'and its methods mutate it';
is sub { my %h = a => 1; my \k = 'a'; %h{k} = 2; %h<a> }(), 2, 'used as a key';
is sub { my \k = 5; do { my \k = 7; k } + k }(), 12, 'an inner same-named term';
is died({ sub { my \k = 5; -> { k = 4 }() }() }), 'X::Assignment::RO', 'a closure capturing it';
is sub { my @seen; for ^3 { my \k = $_ * 2; @seen.push: k }; @seen.join(',') }(), '0,2,4',
    're-declared on every iteration';

sub with-closure(Mu \M) {
    my \key = M.^name ~ '|';
    my $f = { M.^name };
    key ~ $f()
}
is with-closure(Int), 'Int|Int', 'a routine that also makes a closure';
is died({ sub (Mu \M) { my \key = M.^name; my $f = { 1 }; key = 'x' }(Int) }), 'X::Assignment::RO',
    'and still refuses the write';

sub calls(Int $n) { my $s = 0; for ^$n { my \k = $_; $s += k }; $s }
is calls(100), 4950, 'many declarations in a loop';

is died({ sub { my \k = 5; k = 3; CATCH { default { die $_ } } }() }), 'X::Assignment::RO',
    'the error is catchable';
my $message = (try { sub { my \k = 5; k = 3 }(); 'lived' }) // $!.message;
is $message, 'Cannot modify an immutable Int (5)', 'and names the value';
