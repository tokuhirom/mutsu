use v6;
use Test;

plan 26;

# ---------------------------------------------------------------------------
# A colon call with an EMPTY argument list is just a zero-argument call.
# `say 4.log:   ;` is exactly `say 4.log;` in Raku.
# ---------------------------------------------------------------------------

my $plain = 4.log;

my $before-semi = 4.log:   ;
is $before-semi, $plain, 'colon call with no arguments before `;` is a plain call';

my $in-parens = (4.log: );
is $in-parens, $plain, 'colon call with no arguments before `)` is a plain call';

my $in-block = do { 4.log: };
is $in-block, $plain, 'colon call with no arguments before `}` is a plain call';

my @in-brackets = [ 4.log: ];
is @in-brackets.elems, 1, 'colon call with no arguments before `]` is a plain call';

sub double($x) { $x * 2 }
is double(4.log: ), $plain * 2, 'zero-argument colon call works as a sub argument';

is (4.log: ).WHAT.gist, '(Num)', 'zero-argument colon call returns the method result, not a list';

my $upper = 'abc';
$upper .= uc: ;
is $upper, 'ABC', '`.= method:` with no arguments is a zero-argument call';

my %empty-push;
%empty-push.push: ;
is %empty-push.elems, 0, 'zero-argument colon call on a mutating listop method';

# ---------------------------------------------------------------------------
# A colon call WITH arguments still passes them.
# ---------------------------------------------------------------------------

is (4.log: 2), 2, 'colon call passes a single positional argument';
is ((1, 2, 3).join: '-'), '1-2-3', 'colon call passes a string argument';
is-deeply ((1, 2, 3).map: * + 1).List, (2, 3, 4), 'colon call passes a block/WhateverCode';

my @pushed;
@pushed.push: 1, 2, 3;
is-deeply @pushed.List, (1, 2, 3), 'colon call passes a comma list to a listop method';

# ---------------------------------------------------------------------------
# A trailing `.method` written after the last argument of a colon call binds to
# THAT ARGUMENT, because the colon-call argument list is a low-precedence
# listop. This is one of Raku's documented gotchas
# (`raku-doc/doc/Language/syntax.rakudoc`): `$band.substr: 0, 3 .uc` compiles to
# `$band.substr(0, 3.uc)` -- i.e. `$band.substr(0, "3")`.
# ---------------------------------------------------------------------------

my $band = 'Foo Fighters';

is $band.substr( 0, 3 ).uc, 'FOO',
    'parenthesized call: a trailing .uc binds to the whole call result';

is ($band.substr: 0, 3  .uc), 'Foo',
    'colon call: a trailing .uc binds to the last argument, not the call';

is ($band.substr: 0, 1+2 .chars), 'Fo',
    'a trailing .method binds to the last TERM of a complex last argument';

is (($band.substr( 0, 1+2 )).chars), 3,
    'the parenthesized counterpart applies .chars to the whole call result';

is ('abc'.substr: 1 .succ), 'c',
    'single-argument colon call: a trailing .succ binds to the argument';

is ('abc'.substr( 1 ).succ), 'bd',
    'single-argument parenthesized call: a trailing .succ binds to the result';

# The above only reproduces if `substr` numifies a Cool length argument the way
# rakudo does (`"3"` is 3, `True` is 1, `"3.7"` truncates to 3).
is $band.substr(0, '3'), 'Foo', 'substr numifies a Str length argument';
is $band.substr('3.7', 2), ' F', 'substr numifies a Str offset argument';
is $band.substr(0, True), 'F', 'substr numifies a Bool length argument';

# ---------------------------------------------------------------------------
# Colonpairs / adverbs.
# A colon with a SPACE before it is an adverb (named argument), not a colon
# call, and the comma after such an adverb belongs to the enclosing list.
# ---------------------------------------------------------------------------

class Adverbial {
    method m($p = 'x', :$n = 'y') { "$p/$n" }
}
my $obj = Adverbial.new;

is ($obj.m :n<z>), 'x/z', 'a space-separated colonpair is a named argument, not a colon call';
is ($obj.m: :n<z>), 'x/z', 'a colon call whose only argument is a colonpair';
is ($obj.m: 'p', :n<z>), 'p/z', 'a colon call with a positional and a trailing adverb';

is-deeply ("a,,b".split: ',', :skip-empty).List, ('a', 'b'),
    'a colon call keeps a trailing adverb as a named argument';

# A `:2<...>` radix literal after the colon is an ARGUMENT (a term), not an
# adverb or a colon-call terminator.
is (4.log: :2<100>), 1, 'a radix literal is accepted as a colon-call argument';

done-testing;
