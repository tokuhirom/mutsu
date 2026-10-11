use Test;
plan 12;

# `Code.Str` is a method-table row (ADR-12523 slice 6): the routine's name,
# with the warning Rakudo's `Code.Str` emits. Checked against raku.
sub str-of(Mu $code) {
    my @warnings;
    my $str = do {
        CONTROL { when CX::Warn { @warnings.push(.message); .resume } }
        $code.Str
    };
    ($str, @warnings.join("|"))
}

sub foo($x) { }
multi bar(Int) { }
multi bar(Str) { }

is-deeply str-of(&foo), ("foo", "Sub object coerced to string (please use .gist or .raku to do that)"),
    'a named sub';
is-deeply str-of(sub ($a) { }), ("", "Sub object coerced to string (please use .gist or .raku to do that)"),
    'an anonymous sub';
is-deeply str-of({ 1 }), ("", "Block object coerced to string (please use .gist or .raku to do that)"),
    'a bare block';
is-deeply str-of(-> $a { }), ("", "Block object coerced to string (please use .gist or .raku to do that)"),
    'a pointy block';
is-deeply str-of(&bar), ("bar", "Sub object coerced to string (please use .gist or .raku to do that)"),
    'a multi dispatcher';
is-deeply str-of(&say), ("say", "Sub object coerced to string (please use .gist or .raku to do that)"),
    'a built-in routine';
is-deeply str-of(/a/), ("", "Regex object coerced to string (please use .gist or .raku to do that)"),
    'a regex';

# The other renderings do not warn.
is &foo.gist, "&foo", 'gist';
is &foo.name, "foo", 'name';
ok &foo.raku.starts-with("sub foo"), 'raku';
my $renamed = sub { };
$renamed.set_name("renamed");
is-deeply str-of($renamed)[0], "renamed", 'Str follows set_name';
ok str-of(&foo)[0] ~~ Str, 'the answer is a Str';
