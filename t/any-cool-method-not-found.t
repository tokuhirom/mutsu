use v6;
use Test;

# #7773: `Any` is not a `Cool`, so a `Cool` method called on an undefined
# (`Any`) invocant is `X::Method::NotFound` -- not a call served by
# stringifying the type object. mutsu used to answer `$x.comb(/\w+/)` with
# `("Any",)`, built out of the receiver's own gist, which is worse than a
# missing method: the wrong value travels on into the caller's data.

plan 26;

# ── the reported repro ────────────────────────────────────────────────
my $x;
throws-like { $x.comb(/\w+/) }, X::Method::NotFound,
        message => /"No such method 'comb' for invocant of type 'Any'"/,
        'a Cool method on an undefined Any throws';

my $caught;
try {
    my $y;
    $y.comb(/\w+/);
    CATCH { default { $caught = $_ } }
}
is $caught.method, 'comb', 'X::Method::NotFound.method names the call';
is $caught.typename, 'Any', 'X::Method::NotFound.typename is Any';

# The shape that matters: the bad value used to end up in the array.
my $formatted-text;
my @words = "Abe", "Lincoln";
throws-like { @words.push("said", $formatted-text.comb(/\w+/)) },
        X::Method::NotFound, 'the doc example throws instead of pushing (Any)';
is-deeply @words, ["Abe", "Lincoln"], 'nothing was pushed';

# ── the rest of the Cool surface ──────────────────────────────────────
for <uc chars trim substr abs sqrt IO Rat bytes> -> $name {
    my $undef;
    throws-like { $undef."$name"() }, X::Method::NotFound,
            message => /"for invocant of type 'Any'"/,
            "Cool method .$name on an undefined Any throws";
}

# `Mu` has no more of the Cool surface than `Any` does.
throws-like { Mu.uc }, X::Method::NotFound,
        message => /"for invocant of type 'Mu'"/,
        '.uc on Mu throws too';

# ── controls: an unknown name still throws the same way ───────────────
throws-like { my $u; $u.no-such-method }, X::Method::NotFound,
        message => /"No such method 'no-such-method' for invocant of type 'Any'"/,
        'an unknown method on Any is unchanged';

# ── controls: Mu/Any's OWN methods still resolve ──────────────────────
my $u;
is $u.defined, False, '.defined still resolves on an undefined Any';
is $u.gist, '(Any)', '.gist still resolves';
is $u.raku, 'Any', '.raku still resolves';
is $u.^name, 'Any', '.^name still resolves';
is $u.WHAT.gist, '(Any)', '.WHAT still resolves';
ok $u.WHICH.defined, '.WHICH still resolves';
is $u.elems, 1, '.elems still resolves';

# ── controls: a real Cool receiver is untouched ───────────────────────
is "abc".uc, 'ABC', 'a defined Str still answers .uc';
is-deeply "a b".comb(/\w+/).List, ("a", "b"), 'a defined Str still answers .comb';
is 42.abs, 42, 'a defined Int still answers .abs';
