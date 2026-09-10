use v6;
use Test;

# `Code.line` / `Code.file` report where a routine was DECLARED.
#
# Every assertion below is written against *relative* facts (line deltas,
# ordering, basename) so the file can gain a header or move without breaking.
# The file must pass under both `raku` and `mutsu`.

plan 27;

# ---------------------------------------------------------------- named subs

my $anchor = $?LINE;
sub first-sub { 1 }
sub second-sub { 2 }

ok &first-sub.line ~~ Int, 'a named sub .line is an Int';
is &first-sub.line, $anchor + 1, 'a named sub .line is its declarator line';
is &second-sub.line - &first-sub.line, 1,
        'two subs declared one line apart differ by one';

ok &first-sub.file ~~ Str, 'a named sub .file is a Str';
ok &first-sub.file.IO.basename eq 'code-line-file-reflection.t',
        'a named sub .file names this test file';
is &second-sub.file, &first-sub.file,
        'two subs in the same file report the same .file';

# A multi-line declaration reports the line the `sub` keyword sits on, not the
# signature's or the block's.
my $multi-line-anchor = $?LINE;
sub multi-line-decl(
    $x,
    $y,
)
{
    $x + $y
}
is &multi-line-decl.line, $multi-line-anchor + 1,
        'a multi-line declaration reports the `sub` keyword line';

# ------------------------------------------------------------------- methods

my $class-anchor = $?LINE;
class Eatery {
    has $.ingredients;
    method eat { 'nom' }
    method drink { 'glug' }
}
class Bistro is Eatery { }

is Eatery.^lookup('eat').line, $class-anchor + 3,
        'a method .line is its declarator line';
is Eatery.^lookup('drink').line - Eatery.^lookup('eat').line, 1,
        'two methods declared one line apart differ by one';
is Eatery.^lookup('eat').file, &first-sub.file,
        'a method .file is the declaring file';
is Bistro.^lookup('eat').line, Eatery.^lookup('eat').line,
        'an inherited .^lookup reports the DECLARING class location';
is Bistro.^lookup('eat').file, Eatery.^lookup('eat').file,
        'an inherited .^lookup reports the declaring file';

my $submethod-anchor = $?LINE;
class WithSub {
    submethod only-here { 1 }
}
is WithSub.^lookup('only-here').line, $submethod-anchor + 2,
        'a submethod reports its declarator line';

my $role-anchor = $?LINE;
role Seasoned {
    method season { 'salt' }
}
class Dish does Seasoned { }
is Dish.^lookup('season').line, $role-anchor + 2,
        'a role method keeps the role declaration line after composition';

# --------------------------------------------------------- blocks / closures

my $block-anchor = $?LINE;
my $anon = sub { 3 };
my $bare = { 4 };
my $pointy = -> $x { $x };

is $anon.line, $block-anchor + 1, 'an anonymous sub reports its own line';
is $bare.line, $block-anchor + 2, 'a bare block reports its own line';
is $pointy.line, $block-anchor + 3, 'a pointy block reports its own line';
is $anon.file, &first-sub.file, 'an anonymous sub reports the declaring file';
is $bare.file, &first-sub.file, 'a bare block reports the declaring file';

# ------------------------------------------------------------ multi families

my $mm-anchor = $?LINE;
multi sub mm(Int $x) { $x }
multi sub mm(Str $x) { $x }

is &mm.candidates.elems, 2, 'the multi family has two candidates';
is &mm.candidates[0].line, $mm-anchor + 1,
        'the first multi candidate reports its own declarator line';
is &mm.candidates[1].line - &mm.candidates[0].line, 1,
        'the two multi candidates are one line apart';
is &mm.candidates[0].file, &first-sub.file,
        'a multi candidate reports the declaring file';

# ---------------------------------------------------- the .can/.^can contract

ok &first-sub.can('line'), 'a Code object .can("line")';
ok &first-sub.can('file'), 'a Code object .can("file")';
ok Sub.^can('line'), 'Sub.^can("line")';

# ------------------------------------------------------------------- ordering

ok &second-sub.line > &first-sub.line,
        'a later declaration reports a strictly greater line';

done-testing;
