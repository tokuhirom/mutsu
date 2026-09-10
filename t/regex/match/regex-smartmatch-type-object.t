use Test;

# A regex smartmatch stringifies its subject in ordinary STRING CONTEXT.
# For a type object that means: a user `.Stringy`/`.Str` dispatches, and a bare
# type object coerces to "" (with rakudo's uninitialized-value warning), so the
# type NAME is never what the regex is matched against.
#
# Verified assertion-for-assertion against rakudo.

plan 23;

class WithStr    { method Str     { 'foo' } }
class WithStringy { method Stringy { 'bar' } }
class BothCoercions { method Stringy { 'bar' }; method Str { 'baz' } }
class Plain      { }

# --- a type object with a user Str ---------------------------------------
ok  (WithStr ~~ /foo/),  'type object with .Str matches its .Str result';
nok (WithStr ~~ /WithStr/),
    'type object with .Str does NOT match its own type name';
ok  (WithStr.new ~~ /foo/), 'instance with .Str still matches its .Str result';

# An anonymous class has no usable name at all, so this is the shape
# roast/S24-testing/14-like-unlike.t exercises through `like`.
my $anon = class { method Str { 'zap' } };
ok  ($anon ~~ /zap/), 'anonymous type object with .Str matches its .Str result';

# --- .Str, NOT .Stringy, is the coercion a regex match uses ---------------
# Unlike prefix `~` (which is .Stringy), the match target is the .Str
# coercion, so a class defining only .Stringy stringifies to "" here.
quietly {
    nok (WithStringy ~~ /bar/),
        'a type object with only .Stringy does NOT match its .Stringy result';
    nok (WithStringy ~~ /WithStringy/),
        'a type object with only .Stringy does NOT match its own type name';
}
ok  (BothCoercions ~~ /baz/),
    'a type object defining both matches its .Str result';
nok (BothCoercions ~~ /bar/),
    'a type object defining both does NOT match its .Stringy result';

# --- a bare type object coerces to "" -------------------------------------
# The coercion warns, so run these quietly: what is under test is the ANSWER.
quietly {
    nok (Plain ~~ /Plain/), 'bare type object does not match its own type name';
    nok (Int   ~~ /Int/),   'Int does not match /Int/';
    nok (Any   ~~ /Any/),   'Any does not match /Any/';
    nok (Str   ~~ /Str/),   'Str does not match /Str/';

    my $undeclared-value;
    nok ($undeclared-value ~~ /Any/),
        'an uninitialized scalar does not match its type name either';

    # "" is what it coerces to, so an anchored-empty pattern still matches.
    ok (Plain ~~ /^$/), 'bare type object coerces to the empty string';
}

# The coercion is a warning, not a failure, and it names the type.
{
    my $warning;
    {
        CONTROL { default { $warning = .message; .resume } }
        my $ignored = Int ~~ /Int/;
    }
    ok $warning.defined, 'coercing a bare type object for a regex warns';
    ok $warning.contains('Int'), 'the warning names the type';
    ok $warning.contains('string context'), 'the warning is the string-context one';
}

# --- an IMPLICIT-topic match coerces quietly ------------------------------
# `Any ~~ /a/` warns, but a bare `/a/` against an undefined topic does not --
# rakudo distinguishes the synthesized topic from a written-out one. Both
# answer the same; only the warning differs.
{
    my @warnings;
    {
        CONTROL { default { @warnings.push: .message; .resume } }
        my $bare = ?/a/;            # implicit topic, undefined
        is $bare, False, 'a bare regex against an undefined topic is False';
    }
    is @warnings.elems, 0, 'a bare regex against an undefined topic does not warn';
}
{
    my @warnings;
    {
        CONTROL { default { @warnings.push: .message; .resume } }
        my $explicit = ?($_ ~~ /a/);
        is $explicit, False, 'an explicit match on an undefined topic is False too';
    }
    is @warnings.elems, 1, 'but the explicit form does warn';
}

# --- values that are not type objects are untouched -----------------------
ok  (42  ~~ /42/), 'an Int value still matches its own text';
ok  (1.5 ~~ /\.5/), 'a Rat value still matches its own text';

# vim: expandtab shiftwidth=4
