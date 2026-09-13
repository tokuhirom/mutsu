use v6;
use Test;

# A trailing comma in an argument list is an empty list slot, so a statement
# modifier may follow it (`die "x", if @c` — see
# t/control/trailing-comma-before-statement-modifier.t). That rule was stated
# three times with different terminator sets: the paren-less listop path knew
# about modifiers, the two colon-argument paths (`obj.m: a, b` and `$x .= m: a`)
# only knew `;` `}` `)` `]`. And the modifier lookahead trimmed *spaces* rather
# than whitespace, so an embedded comment between the comma and the keyword hid
# it from every path:
#
#     self.set-from-file: $!browser, #`[ $.debug ] unless $driver;
#
# (WebDriver2's driver provider.)

plan 11;

class C {
    has @.seen;
    method f(*@a) { @!seen.append: @a; @a.elems }
    method g($guard) {
        self.f: 1, #`[ dropped ] unless $guard;
        self.f: 2, if $guard;
        'done'
    }
}

# The method colon-argument form, with and without a comment in the gap.
{
    my $c = C.new;
    is ($c.f: 1, unless 0), 1, 'a trailing comma before `unless` in a colon-arg list';
    is $c.seen.join(','), '1', 'and the one argument arrived';
}
{
    my $c = C.new;
    is ($c.f: 1, #`[ x ] unless 0), 1, 'an embedded comment between the comma and `unless`';
    is $c.seen.join(','), '1', 'the comment is not an argument';
}
{
    my $c = C.new;
    is $c.g(True), 'done', 'both forms parse inside a method body';
    is $c.seen.join(','), '2', 'the guarded calls ran as their conditions say';
}
{
    my $c = C.new;
    $c.f: 1, 2, #`( trailing ) if 1;
    is $c.seen.join(','), '1,2', 'a multi-argument colon list keeps all of its arguments';
}

# `.=` colon-arg calls take the same rule.
{
    my $s = 'aa';
    $s .= subst: 'a', 'b', unless 0;
    is $s, 'ba', 'a trailing comma before `unless` in a `.=` colon-arg list';
}

# A paren-less listop head with a comment before the modifier.
{
    my @out;
    @out.push: 3;
    lives-ok { EVAL 'my @c; say join ",", 1, #`[ c ] if @c' },
        'a builtin listop head with a comment before `if`';
}

# Things that must NOT change: a trailing comma still closes an argument list
# before a group closer, and a same-named pair key is still a pair.
{
    my $c = C.new;
    is ($c.f: 1, 2,), 2, 'a trailing comma before `)` still ends the list';
    my @a = 1, if => 2;
    is-deeply @a, [1, (if => 2)], 'an `if =>` pair key is still a pair';
}
