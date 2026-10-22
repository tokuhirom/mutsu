use v6.d;
use Test;

plan 8;

# Rakudo checks the positional arity before it looks for a missing required
# named parameter, so a surplus positional is the error reported.
class P {
    method g(:$a!) { $a }
    method !h(:$a!) { $a }
    method callh { self!h(1) }
}
sub f(:$a!) { }
sub g($x, :$a!) { }

sub error-of(&code) {
    try code();
    $!.defined ?? "{$!.^name}: {$!.message}" !! 'no error';
}

# A call that cannot be resolved at compile time goes through a variable.
my &fc = &f;
my &gc = &g;

is error-of({ fc(1) }), 'X::AdHoc: Too many positionals passed; expected 0 arguments but got 1',
    'a sub with only a required named: the surplus positional wins';
is error-of({ fc(1, 2) }), 'X::AdHoc: Too many positionals passed; expected 0 arguments but got 2',
    '... with two surplus positionals';
is error-of({ gc(1, 2) }), 'X::AdHoc: Too many positionals passed; expected 1 argument but got 2',
    'one positional parameter reads "1 argument"';
is error-of({ gc(1) }), 'X::AdHoc: Required named parameter \'a\' not passed',
    'with no surplus the missing named parameter is still reported';
is error-of({ fc() }), 'X::AdHoc: Required named parameter \'a\' not passed',
    'no positionals at all reports the missing named parameter';
is error-of({ fc(:a(1)) }), 'no error', 'a supplied named parameter binds';

# The invocant counts in Rakudo's message for a method; mutsu leaves it out
# (#14565), so only the class and the start of the message are pinned.
ok error-of({ P.g(1) }).starts-with('X::AdHoc: Too many positionals passed'),
    'a method: the surplus positional wins';
ok error-of({ P.new.callh }).starts-with('X::AdHoc: Too many positionals passed'),
    'a private method: the surplus positional wins';
