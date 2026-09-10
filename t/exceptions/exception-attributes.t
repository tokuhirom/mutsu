use Test;

# Every `X::` attribute here was previously unanswerable, so `throws-like`
# printed `# SKIPPED matcher '.<name>': mutsu's <class> carries no such
# attribute` instead of running the check (see
# news/2026-08/throws-like-named-matchers-no-longer-silently-skipped.md).
# Each assertion below is one of those SKIPPED notices turned into a real one.
#
# Every expectation was read off `raku`'s own metamodel first
# (`.^attributes`, then the attribute's value in a concrete throwing case) —
# none were invented to fit mutsu.

plan 21;

# --- X::Anon::Multi: .multiness / .routine-type -----------------------------
throws-like 'multi sub () { }', X::Anon::Multi, multiness => 'multi';
throws-like 'proto sub () { }', X::Anon::Multi, multiness => 'proto';
throws-like 'class { multi method () { }}', X::Anon::Multi,
    multiness => 'multi', routine-type => 'method';

# --- X::Syntax::Number::RadixOutOfRange: .radix -----------------------------
throws-like ':45<abcd>', X::Syntax::Number::RadixOutOfRange,
    radix => 45, message => 'Radix 45 out of range (allowed: 2..36)';
throws-like ':1<0>', X::Syntax::Number::RadixOutOfRange, radix => 1;
# The run-time twin (`.parse-base`) shares the type, the attribute and rakudo's
# wording; it used to spell an older rakudo message.
throws-like { "z".parse-base(45) }, X::Syntax::Number::RadixOutOfRange,
    radix => 45, message => 'Radix 45 out of range (allowed: 2..36)';

# --- X::Syntax::Variable::Numeric: .what ------------------------------------
throws-like 'my $0', X::Syntax::Variable::Numeric, what => 'variable';
throws-like 'my sub f($0) { }', X::Syntax::Variable::Numeric, what => 'parameter';

# --- X::Syntax::Missing: .what ----------------------------------------------
# rakudo's `.what` is the whole tail of `Missing {what}`, so BOTH of roast's
# regexes (/initializer/ and /constant/) match this one diagnosis.
throws-like 'constant foo;', X::Syntax::Missing, what => /initializer/;
throws-like 'constant * = 3;', X::Syntax::Missing, what => /constant/;

# --- X::Method::InvalidQualifier: .method / .invocant / .qualifier-type ------
# `.invocant` is the VALUE and `.qualifier-type` the TYPE OBJECT, neither of
# which the message text carries.
throws-like '1.List::join', X::Method::InvalidQualifier,
    method => 'join', invocant => 1, qualifier-type => List;

# --- X::Syntax::CannotMeta: .meta / .operator / .reason ---------------------
throws-like { Q/my $a; $a R[and]= 42/.EVAL }, X::Syntax::CannotMeta,
    meta => "reverse the args of", operator => "=", reason => "too fiddly";
throws-like { Q/my $a; $a X[or]= 42/.EVAL }, X::Syntax::CannotMeta,
    meta => "cross with", operator => "=", reason => "too fiddly";

# --- X::Syntax::DuplicatedPrefix: .prefixes ---------------------------------
throws-like "1%^^1", X::Syntax::DuplicatedPrefix, prefixes => "^^";
throws-like "555 ~~!~~ 666", X::Syntax::DuplicatedPrefix, prefixes => "~~";

# --- X::Trait::Invalid: .type / .subtype ------------------------------------
throws-like ｢sub foo($x? is rw) {}｣, X::Trait::Invalid,
    :type('is'), :subtype('rw'), :name('$x'),
    message => "Cannot use 'is rw' on optional parameter '\$x'.";

# --- the negative case: prove these matchers really can FAIL -----------------
#
# Everything above only shows the attributes now *answer*. A skipped matcher
# also "passes", so a test that checked only the passing direction would be
# exactly as vacuous as the bug this descends from. Run the failing direction
# in a child process (a failing `throws-like` here would fail this file) and
# assert the child's PLAN LINE too: a skipped matcher plans `1..2` where a real
# one plans `1..3`.

my $script = '_exception_attributes_child.raku';
spurt $script, q:to/END/;
use Test;
plan 4;
throws-like 'multi sub () { }', X::Anon::Multi, multiness => 'NOPE';
throws-like ':45<abcd>', X::Syntax::Number::RadixOutOfRange, radix => 999;
throws-like '1.List::join', X::Method::InvalidQualifier, qualifier-type => Hash;
throws-like 'my sub f($0) { }', X::Syntax::Variable::Numeric, what => 'variable';
END

my $proc = run($*EXECUTABLE, $script, :out, :!err);
my $out := $proc.out.slurp;
my $exit = $proc.exitcode;

is +$out.comb(/ ^^ \s* '1..3' /), 4,
    'each subtest plans 2 + its one matcher (1..3), i.e. nothing was skipped';
like $out, / ^^ \s* 'not ok 3 - .multiness matches NOPE' /,
    'a wrong .multiness actually FAILS';
like $out, / ^^ \s* 'not ok 3 - .radix matches 999' /,
    'a wrong .radix actually FAILS';
like $out, / ^^ \s* 'not ok 3 - .qualifier-type matches' /,
    'a wrong .qualifier-type actually FAILS';
isnt $exit, 0, 'the child run reports failure';

unlink $script;

# vim: expandtab shiftwidth=4
