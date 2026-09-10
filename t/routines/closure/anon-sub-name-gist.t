use v6;
use Test;
use lib $?FILE.IO.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

plan 19;

# PLAN §8.13 leftovers: `anon sub NAME {...}` keeps its .name without
# installing `&NAME`, and Sub gist/Str/raku render the Rakudo forms.

my $s = anon sub foo { 42 };
is $s.name, 'foo', 'anon sub NAME keeps its .name';
is $s.(), 42, 'anon sub is callable';
is $s.gist, '&foo', 'named Sub gists as &name';
like $s.raku, /^ 'sub foo { #`(Sub|' \d+ ') ... }' $/, 'named Sub .raku form';

my $t = anon sub add($x, $y) { $x + $y };
is $t.name, 'add', 'anon sub with params keeps its .name';
is $t.(2, 3), 5, 'anon sub with params is callable';

# `anon` must NOT install the name in the enclosing scope.
eval-dies-ok 'my $a = anon sub nowhere-installed { 1 }; nowhere-installed()',
    'anon sub name is not installed';
sub outer { 'outer' }
my $shadow = anon sub outer { 'anon' };
is outer(), 'outer', 'anon sub does not clobber an existing same-named sub';
is $shadow.(), 'anon', 'the anon value itself stays callable';

# Anonymous declarations accept the same Unicode identifier starts as regular
# declarations, and say uses the named Sub's gist form.
my $unicode-sub = anon sub þ { 42 };
is $unicode-sub.name, 'þ', 'anon sub keeps a non-ASCII name';
is $unicode-sub.(), 42, 'anon sub with a non-ASCII name is callable';
is (anon class þ {}).^name, 'þ', 'anon class accepts a non-ASCII name';
is_run 'say anon sub Foo { 42 };', { :out("&Foo\n") },
    'say uses the named anon sub gist';
is_run 'say anon sub þ { 42 };', { :out("&þ\n") },
    'say uses the non-ASCII anon sub gist';

# Sub rendering forms (Rakudo-compatible).
sub named-here { }
is &named-here.gist, '&named-here', 'a declared sub gists as &name';
is &named-here.Str, 'named-here', 'Sub.Str is the bare name';
is (sub { }).gist, 'sub { }', 'anonymous sub gists as sub { }';
like (sub ($x) { }).raku, /^ 'sub ($x) { #`(Sub|' \d+ ') ... }' $/,
    'anonymous sub .raku keeps its signature';
like (sub { }).raku, /^ 'sub { #`(Sub|' \d+ ') ... }' $/,
    'anonymous sub .raku omits an empty signature';

done-testing;
