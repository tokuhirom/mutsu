use Test;

# A statement-position call with named arguments compiles to the same call
# as the expression form (#9462): the retired `ExecCallPairs` opcode was a
# second copy of call dispatch.

plan 12;

sub w(:$x) { warn "w$x"; "after" }
my @seen;
{
    w :x(1);
    @seen.push: 'resumed';
    CONTROL { default { @seen.push: .message; .resume } }
}
is-deeply @seen, ['w1', 'resumed'], 'a warn inside a named-arg statement call resumes after the call';

my %h;
lives-ok { %h<a> = 42 }, 'a block Test function';
is %h<a>, 42, '... writes through to the caller';

sub tail(:$v) { $v * 2 }
sub f() { tail :v(21) }
is f(), 42, 'a tail-position named-arg call is the routine value';

my @log;
sub note-it(:$m) { @log.push: $m; 'discarded' }
note-it :m<a>;
note-it m => 'b';
is-deeply @log, [<a b>], 'named-arg statement calls run for their effect';

sub slurp-all(*@a, *%h) { @log = |@a, |%h.sort».kv.flat }
slurp-all 1, |(2, 3), :k<v>;
is-deeply @log, [1, 2, 3, 'k', 'v'], 'a slip and a named argument in a statement call';

sub f2() { note-it :m<c>; 'body value' }
is f2(), 'body value', 'a non-tail named-arg statement call leaves no value behind';

# Bugs the retired opcode's carrier path used to hide.
# (Called through `&` so Rakudo does not reject the call at compile time.)
sub only-named(:$x) { $x }
my &on = &only-named;
dies-ok { on(1, 2) }, 'surplus positionals to an all-named sub die';
throws-like { on(1, 2) }, Exception, message => /'Too many positionals'/,
    '... with the arity message';
{
    my @a;
    dies-ok { push @a, a => 52 }, 'the sub form of push takes no named argument';
    @a.push(a => 52);
    is-deeply @a, [], '... while the method form ignores one';
}
{
    $_ = "12345";
    is-deeply ([3, 4].map: { S{$^a} = 'X' }), ('12X45', '123X5'),
        'S/// in a placeholder block substitutes the outer $_';
}
