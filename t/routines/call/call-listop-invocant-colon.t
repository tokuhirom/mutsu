use Test;

# Raku's listop invocant colon makes the FIRST argument the invocant:
# `foo $x: @args` is `$x.foo(@args)`. mutsu used to drop the colon and call the
# listop normally, which agreed with rakudo only by coincidence (`say`, `sort`,
# `return`) and gave a wrong answer where it did not (`warn "w":` warned instead
# of throwing X::Method::NotFound). `die`/`fail`/`take` did not parse at all,
# because their dedicated statement parsers stopped before the colon.
#
# This file pins the shared rule across the whole family rather than just the
# constructs that happened to be broken. See issue #8141.

plan 16;

# --- listops whose method DOES exist: the colon dispatches it ---------------

my @a = 3, 1, 2;
my $sorted = sort @a:;
is $sorted.List.raku, (1, 2, 3).raku, 'sort @a: dispatches @a.sort';

sub returns_it() { my $x = 42; return $x: }
is returns_it(), 42, 'return $x: dispatches $x.return and returns from the routine';

sub returns_early() { return "r": ; flunk "return \$x: did not transfer control" }
is returns_early(), "r", 'return $x: transfers control like return $x';

sub taker() { take "t": }
my $taken = gather { taker() };
is $taken.List.raku, ("t",).raku, 'take $x: dispatches $x.take';

# `Str` has no `take-rw` method, so the colon throws here exactly as it does
# for `warn`/`die`/`fail` below.
throws-like 'my @r = gather { my $v = "u"; take-rw $v: }', X::Method::NotFound,
    method => 'take-rw', typename => 'Str',
    'take-rw $x: dispatches $x.take-rw';

# The return-type check still fires at the routine boundary, so the method
# dispatch really is routed through the routine's return path.
throws-like 'sub typed(--> Int) { return "r": }; typed()',
    X::TypeCheck::Return,
    'return $x: still type-checks the return value';

# --- listops whose method does NOT exist: the colon must throw --------------

throws-like 'sub g() { warn "w": }; g()', X::Method::NotFound,
    method => 'warn', typename => 'Str',
    'warn $x: is $x.warn, not a warning';

throws-like 'sub f() { die "boom": }; f()', X::Method::NotFound,
    method => 'die', typename => 'Str',
    'die $x: is $x.die, not a die with that message';

throws-like 'sub f() { fail "boom": }; f()', X::Method::NotFound,
    method => 'fail', typename => 'Str',
    'fail $x: is $x.fail, not a fail with that message';

# The same three outside a routine body: the tail-statement compile path used to
# differ from the mid-body one (it panicked on the invocant instead of throwing).
throws-like 'warn "w":', X::Method::NotFound,
    'warn $x: as the final statement of the program';

throws-like 'die "boom":', X::Method::NotFound,
    'die $x: as the final statement of the program';

throws-like 'say "x"; warn "w":', X::Method::NotFound,
    'warn $x: as the final statement after another statement';

# --- the invocant colon carries the remaining arguments ---------------------

class Greeter {
    method greet($a, $b) { "$a-$b" }
}
my $g = Greeter.new;
is (greet $g: "x", "y"), "x-y", 'listop invocant colon passes the remaining args';

# --- a colonpair is NOT an invocant colon -----------------------------------

sub adverbed($x, :$loud) { $loud ?? uc($x) !! $x }
is adverbed("q", :loud), "Q", 'a leading colonpair adverb is not an invocant colon';

sub plain_ret() { return 5 }
is plain_ret(), 5, 'plain return still works';

# `::` in a type name must not be read as an invocant colon either.
throws-like 'die X::AdHoc.new(payload => "p")', X::AdHoc,
    'die X::Name.new(...) is not affected by the invocant-colon rule';
