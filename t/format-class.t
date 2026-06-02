use Test;

# The Format class (Raku 6.e): a Callable wrapper around a sprintf format string.
# Mirrors the runnable (non-RakuAST) parts of roast/S32-str/format.t.

plan 40;

# --- argless format ---------------------------------------------------------
my $f0 = Format.new("foo");
isa-ok $f0, Format,                  'Format.new returns a Format';
ok     $f0 ~~ Format,                'a Format smartmatches Format';
isa-ok $f0.Callable, Callable,       '.Callable is a Callable';
isa-ok $f0.Callable.signature, Signature, '.Callable.signature is a Signature';
is     $f0.arity, 0,                 'arity of argless format';
is     $f0.count, 0,                 'count of argless format';
is     $f0(),     'foo',             'argless format is callable';
is     "$f0",     'foo',             'argless format stringifies to the format';
is     "$f0()",   'foo',             'argless format call interpolates';

is     ().fmt($f0), '',              'empty list is fine for argless format';

# argless format over a non-empty list throws an arity error
my $err;
{ <a b c>.fmt($f0); CATCH { default { $err = $_ } } }
isa-ok $err, X::Str::Sprintf::Directives::Count, 'argless over list throws Count';
is     $err.args-have, 0,            'args-have is 0';
is     $err.args-used, 1,            'args-used is 1';
is     $err.format,   'foo',         'format is foo';

# --- one-arg format ---------------------------------------------------------
my $f1 = Format.new("%5s");
isa-ok $f1, Format,                  'one-arg Format';
is     $f1.arity, 1,                 'arity one arg';
is     $f1.count, 1,                 'count one arg';
is     $f1("foo"),     '  foo',      'one-arg format is callable';
is     "$f1",          '%5s',        'one-arg format stringifies';

# --- two-arg format ---------------------------------------------------------
my $f2 = Format.new("%5s:%5s");
is     $f2.arity, 2,                 'arity two args';
is     $f2.count, 2,                 'count two args';
is     $f2("foo","bar"),     '  foo:  bar', 'two-arg format is callable';
is     "$f2",                '%5s:%5s',     'two-arg format stringifies';

# --- .fmt over collections with a one-arg format ----------------------------
for <Set SetHash Bag BagHash Mix MixHash> -> $coercer {
    is <a b b>."$coercer"().fmt($f1, ':'), '    a:    b' | '    b:    a',
      "fmt on $coercer with one-arg format";
}

# --- .fmt over Set/SetHash with a two-arg (kv) format -----------------------
for <Set SetHash> -> $coercer {
    is <a b b>."$coercer"().fmt($f2, ','),
      '    a: True,    b: True' | '    b: True,    a: True',
      "fmt on $coercer with two-arg format";
}

# --- .fmt over Bag/Mix with a two-arg (kv) format ---------------------------
for <Bag BagHash MixHash> -> $coercer {
    is <a b b>."$coercer"().fmt($f2, ','),
      '    a:    1,    b:    2' | '    b:    2,    a:    1',
      "fmt on $coercer with two-arg format";
}

# --- .fmt over a Map --------------------------------------------------------
my $map := Map.new( (a => 1, b => 2) );
is $map.fmt($f1, ':'), '    a:    b' | '    b:    a', 'fmt on Map with one arg';
is $map.fmt($f2, ','),
  '    a:    1,    b:    2' | '    b:    2,    a:    1', 'fmt on Map with two args';

# --- .fmt over List/Seq (positional, ordered, batched) ----------------------
my $list := <a a b b>;
for <List Seq> -> $coercer {
    is $list."$coercer"().fmt($f1, ':'), '    a:    a:    b:    b',
      "fmt on $coercer with one-arg format";
    is $list."$coercer"().fmt($f2, ','), '    a:    a,    b:    b',
      "fmt on $coercer with two-arg format";
}

# vim: expandtab shiftwidth=4
