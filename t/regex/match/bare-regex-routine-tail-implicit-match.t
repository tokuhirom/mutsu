use Test;

# A bare regex literal (no `m`/`rx` prefix) is a statement in its own right in
# Raku: wherever it appears as a full statement -- including as the final
# ("tail") statement of a Sub/Block, whose value becomes the routine's
# implicit return -- it desugars to `$_ ~~ /regex/`, not to a bare `Regex`
# value. This was already handled for an `if`/`while` condition and for an
# ordinary sunk statement (both go through `Compiler::compile_condition_expr`
# in src/compiler/stmt.rs), but NOT for a routine's tail statement
# (`Compiler::compile_routine_tail_expr` in src/compiler/helpers_sub_body.rs
# called plain `compile_expr`, which just loads the `Regex` value). Since a
# `Regex` object is always truthy regardless of whether it actually matched,
# a predicate block whose sole statement is a bare regex -- the shape
# `dir(:test)` callers commonly use, e.g. `dir($path, test => { /\.html$/ })`
# in the `Data::UkraineWar::MoD` distribution -- never rejected anything.

plan 6;

my $test = { /\.html$/ };
ok $test('foo.html'), 'block whose sole statement is a bare regex matches a matching arg';
nok $test('foo.txt'), 'block whose sole statement is a bare regex rejects a non-matching arg';
nok $test('.'), 'and rejects "." (dir()s default entries)';
nok $test('..'), 'and rejects ".." too';

# The `dir(:test)` predicate itself, end to end: a custom `:test` block that
# would only accept `*.html` names must not let `.`/`..` slip through.
mkdir 'tmp' unless 'tmp'.IO.d;
my $dir = 'tmp/bare-regex-routine-tail-implicit-match';
mkdir $dir unless $dir.IO.d;
spurt "$dir/keep.html", "x";
spurt "$dir/skip.txt", "x";
my @found = dir($dir, test => { /\.html$/ }).sort(*.basename);
is-deeply @found.map(*.basename).List, ('keep.html',), 'dir(:test) with a bare-regex block excludes "." and non-matches';

my @entries = dir($dir, test => { /\.html$/ });
is @entries.elems, 1, 'dir(:test) never returns the "." / ".." entries for a bare-regex predicate';

unlink "$dir/keep.html";
unlink "$dir/skip.txt";
rmdir $dir;
