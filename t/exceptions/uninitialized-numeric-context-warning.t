use Test;

# An undefined operand of a numeric infix op (`$q + 1`, `$q == 0`) warns
# "Use of uninitialized value $q of type Any in numeric context" as a
# resumable CX::Warn, naming the variable the way rakudo does, and
# numifies to 0 (#9359).

plan 13;

sub warnings-of(&code) {
    my @w;
    code();
    CONTROL { when CX::Warn { @w.push: .message.lines.head; .resume } }
    @w
}

my $q;
is-deeply warnings-of({ my $r = $q + 1 }),
    ['Use of uninitialized value $q of type Any in numeric context'],
    'infix + names the variable';
is warnings-of({ my $r = 1 - $q }).head,
    'Use of uninitialized value $q of type Any in numeric context',
    'the right-hand operand is named too';

for (
    '*'  => { my $r = $q * 2 },
    '/'  => { my $r = 1 / $q },
    '%'  => { my $r = $q % 3 },
    '==' => { my $r = $q == 0 },
    '<'  => { my $r = $q < 1 },
) -> (:key($op), :value(&code)) {
    is warnings-of(&code).elems, 1, "infix $op warns once";
}

is warnings-of({ my $x; my $r = $x + $x }).elems, 2, 'each undefined operand warns';

sub f($p) { $p + 1 }
is-deeply warnings-of({ f(Any) }),
    ['Use of uninitialized value of type Any in numeric context'],
    'a readonly parameter has no container to name';

is-deeply warnings-of({ my $r = Str + 1 }),
    ['Use of uninitialized value of type Str in numeric context'],
    'a type object literal is unnamed';

{
    my $r;
    my @w = warnings-of({ $r = $q + 5 });
    is $r, 5, 'the undefined operand numifies to 0';
}

class C {
    has $.a;
    method m { $!a + 1 }
}
is-deeply warnings-of({ C.new.m }),
    ['Use of uninitialized value $!a of type Any in numeric context'],
    'an attribute is named with its twigil';

is warnings-of({ my $r = 1 + 2; my $s = 0; $s = $s + 1 }).elems, 0,
    'defined operands do not warn';
