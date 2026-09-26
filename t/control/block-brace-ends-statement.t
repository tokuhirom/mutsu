use Test;

# In Raku a `}` that closes a block and sits at the end of a line terminates
# the statement. So a word that would otherwise be an infix operator -- `before`,
# `after`, `eq`, ... -- begins a NEW statement when it starts the next line:
#
#     g { 1 }
#     before { 2 }      # two calls, not `g({ 1 } before { 2 })`
#
# Without this rule, `Cro::HTTP::Router`'s idiomatic
# `before-matched { ... }` / `before { ... }` stanza inside a `route { }` block
# parsed as one `before-matched(Bool)` call and died with
# "No matching candidates for proto sub: before-matched".

plan 12;

my $got;
sub g($x) { $got = $x }
sub before($x) { $x }
sub after($x) { $x }

$got = Nil;
g { 1 }
before(5);
ok $got ~~ Callable, 'a block argument survives a following `before` line';

$got = Nil;
g { 1 }
after(5);
ok $got ~~ Callable, 'a block argument survives a following `after` line';

# The rule is about end of *line*: on one line the infix really is an infix.
ok ({ 1 } before { 2 }).WHAT === Bool, 'same-line `before` is still the infix';

# It is the brace that terminates, not the newline: a non-block operand still
# continues across a line break.
my $sum = 1
    + 2;
is $sum, 3, 'a newline before an infix still continues a non-block expression';

# A `}` followed by something other than a newline does not terminate.
my @a = 1, 2, 3;
ok (@a.map({ $_ }).join(',') eq '1,2,3'), 'a brace mid-line does not terminate';

# Every block term ends the statement the same way, and so does every infix
# that could also begin a term (#9479: PDF::COS::Tie's
#     my &accessor = sub (\obj) is rw { ... }
#     &accessor.set_name( $key );
# parsed as `sub {...} & accessor.set_name(...)` and died with
# "Unknown function: accessor").
{
    my &a = sub ($o) { 1 }
    &a.set_name("k");
    is &a.name, 'k', 'an anonymous sub followed by a `&code` line';

    my $b = -> { 2 }
    &a.name eq 'k' or die;
    ok $b ~~ Block, 'a pointy block followed by a `&code` line';

    my $m = method { 3 }
    -1 == -1 or die;
    ok $m ~~ Method, 'a method literal followed by a `-` line';

    my %h = k => { 4 }
    ~"x" eq "x" or die;
    ok %h<k> ~~ Block, 'a block value followed by a `~` line';

    my $s = sub { 5 }
    %h<k>;
    ok $s ~~ Sub, 'an anonymous sub followed by a `%hash` line';

    my @l = do { 6 }
    |(7, 8);
    is-deeply @l, [6], 'a do block followed by a `|` line';
}

# A subscript is not a block: its `}` does not end the statement.
{
    my %h = a => 1;
    my $v = %h{"a"}
        + 10;
    is $v, 11, 'a `%h{...}` subscript continues across the newline';
}
