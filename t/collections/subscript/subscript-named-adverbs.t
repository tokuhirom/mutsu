use Test;

# Any colonpair after a subscript is an adverb: a named argument to
# `postcircumfix:<[ ]>` / `<{ }>` (or the multi-dimensional `<[; ]>` /
# `<{; }>`). One that is not a built-in subscript adverb fails at run time the
# way rakudo's CORE candidates do (#10292): no candidate for a single
# positional element or a multi-dim subscript (X::Multi::NoMatch), an
# X::Adverb from every slice candidate. Expectations are rakudo's.

plan 29;

my %hash = a => { b => { c => 42 } };
my %h = a => 1, b => 2;
my @a = 1, 2, 3;
my @m = [1, 2], [3, 4];
my $no = False;

# --- the issue's repro: these used to be parse errors ---
is (try %hash{"a";"b";"c"}:$no) // "died", "died", 'multi-dim hash subscript with :$no parses';
is (try @m[1;0]:$no) // "died", "died", 'multi-dim array subscript with :$no parses';
throws-like { %hash{"a";"b";"c"}:$no }, X::Multi::NoMatch, 'multi-dim {; } has no :no candidate';
throws-like { @m[1;0]:foo }, X::Multi::NoMatch, 'multi-dim [; ] has no :foo candidate';
throws-like { @m[0;1]:exists:foo }, X::Multi::NoMatch, 'multi-dim with a built-in and an unknown adverb';

# --- a single positional element: no candidate takes only an unknown adverb ---
throws-like { @a[0]:foo }, X::Multi::NoMatch, '@a[0]:foo';
throws-like { @a[0]:!foo }, X::Multi::NoMatch, '@a[0]:!foo';
throws-like { @a[0]:foo(3) }, X::Multi::NoMatch, '@a[0]:foo(3)';
throws-like { @a[0]:foo<x> }, X::Multi::NoMatch, '@a[0]:foo<x>';
throws-like { @a[0]:$no }, X::Multi::NoMatch, '@a[0]:$no';
throws-like { @a[0]:foo:bar }, X::Multi::NoMatch, '@a[0]:foo:bar';
throws-like { @a[*-1]:foo }, X::Multi::NoMatch, 'a Callable index';
throws-like { @a[0] :foo }, X::Multi::NoMatch, 'whitespace before the adverb';

# --- a built-in adverb with an unknown one: X::Adverb on element access ---
throws-like { @a[0]:k:foo }, X::Adverb, what => 'element access', source => '@a',
    unexpected => <foo>, nogo => <k>, '@a[0]:k:foo';
throws-like { @a[0]:k(0):foo }, X::Adverb, nogo => <!k>, 'a false built-in adverb is reported negated';
throws-like { @a[0]:exists:foo }, X::Adverb, nogo => <exists>, ':exists with an unknown adverb';
throws-like { @a[0]:kv:v:foo }, X::Adverb, nogo => <kv v>, 'several built-in adverbs';
throws-like { @a[0]:delete:foo }, X::Adverb, nogo => <delete>, ':delete with an unknown adverb';
is-deeply @a, [1, 2, 3], '... and nothing was deleted';

# --- slices and every associative subscript: X::Adverb ---
throws-like { @a[0,1]:foo }, X::Adverb, what => 'slice', unexpected => <foo>, nogo => (),
    message => "Unexpected adverb 'foo' passed to slice on '@a'.", 'a positional slice';
throws-like { @a[*]:foo }, X::Adverb, what => 'whatever slice', 'a whatever slice';
throws-like { @a[]:foo }, X::Adverb, what => 'zen slice', 'a zen slice';
throws-like { %h<a>:foo }, X::Adverb, what => 'slice', source => '%h', 'a single hash key';
throws-like { %h{"a"}:$no }, X::Adverb, unexpected => <no>, 'a hash key with :$no';
throws-like { %h{}:foo }, X::Adverb, what => '{} slice', 'a hash zen slice';
my $hr = { a => 1 };
throws-like { $hr<a>:foo(0) }, X::Adverb, source => '$hr', 'a scalar-held hash';
throws-like { @a[0,1]:foo:bar }, X::Adverb,
    message => "2 unexpected adverbs ('bar', 'foo') passed to slice on '@a'.",
    'several unknown adverbs are counted and sorted';

# --- `:$k` is the `k` adverb with a runtime flag ---
my $k = True;
is @a[1]:$k, 1, '@a[1]:$k';
is %h<b>:$k, 'b', '%h<b>:$k';
