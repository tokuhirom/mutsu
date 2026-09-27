use Test;

# The "No return arguments allowed when return value ... is already specified
# in the signature" check only covers a `return` in the routine's own scope.
# Rakudo resets the signature info in every nested block, so a `return` inside
# `if`/`for`/`while`/a bare block is accepted and returns its own argument.
# Statement-modifier forms open no block and are still rejected, and the
# `.return` method checks the enclosing routine's pinned value at run time
# wherever it is called (rakudo's Mu.return -> check-signature).
# Found via the Usage::Utils distribution (`sub say-coloured(... --> True)`
# does `return True` from inside an `if`).

plan 11;

{
    my sub f($x --> True) { if $x { return True }; 1 }
    is f(1), True, 'return inside an if block is allowed';
    is f(0), True, 'falling off the end yields the signature value';
}

{
    my sub g(--> 42) { for 1 { return 7 } }
    is g(), 7, 'return inside a for block returns its own argument';
}

{
    my sub h(--> Nil) { { return 5 } }
    is h(), 5, 'return inside a bare block returns its own argument';
}

{
    my $n = 0;
    my sub w(--> True) { while $n < 3 { $n++; return 3 if $n == 2 } }
    is w(), 3, 'modifier inside a while block is inside a block';
}

{
    my sub k(--> True) { -> { return 3 }() }
    is k(), 3, 'return inside a pointy block is allowed';
}

throws-like 'sub f($x --> True) { return True if $x }; f(1)', X::Comp,
    'if modifier is still in the routine scope', payload => /True/;
throws-like 'sub f($x --> True) { return 1 unless $x }; f(1)', X::Comp,
    'unless modifier is still in the routine scope', payload => /True/;
throws-like 'sub f(--> True) { return 1 for 1..2 }; f()', X::Comp,
    'for modifier is still in the routine scope', payload => /True/;
throws-like 'sub f(--> True) { FOO: return 1 }; f()', X::Comp,
    'labelled return is still in the routine scope', payload => /True/;
throws-like 'sub f(--> 42) { if 1 { 27.return } }; f', X::AdHoc,
    '.return inside a block still checks the pinned value', payload => /42/;
