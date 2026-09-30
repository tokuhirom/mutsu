use Test;

plan 9;

# A `Nil` item of a `state @a = LIST` initializer decays to the container's
# default, exactly as it does for `my @a = LIST` (ADR-0049, #10357).
sub statement-form { state @s = Nil, Any; @s.raku }
is statement-form(), '[Any, Any]', 'statement form: Nil decays to Any';

sub expression-form { my $x = (state @s = Nil, Any); $x.raku }
is expression-form(), '$[Any, Any]', 'expression form: Nil decays to Any';

is (state @t = Nil, 3).raku, '[Any, 3]', 'a Nil item next to a value decays';

# The element type supplies the default for a typed array.
state Int @typed = Nil, 2;
is @typed.raku, 'Array[Int].new(Int, 2)', 'typed: Nil decays to the type object';

# `is default(...)` still wins (handled by the default-first store).
state @d is default(7) = Nil, Any;
is @d.raku, '[7, Any]', 'is default: Nil uses the default, Any stays Any';

# The state persists: only the first entry initializes.
sub persists { state @p = Nil, 1; @p.push(@p.elems); @p.raku }
persists();
is persists(), '[Any, 1, 2, 3]', 'the decayed array persists across entries';

my @seen;
for ^2 {
    state @loop = Nil, Any;
    @seen.push(@loop.raku);
}
is @seen.join(' '), '[Any, Any] [Any, Any]', 'state in a loop body';

# A state array with no Nil item is untouched, and a state hash is not affected.
state @plain = 1, 2;
is @plain.raku, '[1, 2]', 'an initializer without Nil is unchanged';
state %h = a => 1;
is %h.raku, '{:a(1)}', 'a state hash is unchanged';
