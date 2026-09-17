use Test;

# `dir($path, test => { /\.html$/ })` -- the shape `Data::UkraineWar::MoD`'s
# `Data::UkraineWar::MoD::Scrape.new` uses to list only the raw HTML pages in
# a directory -- always included "." and ".." (and, more generally, never
# excluded any entry) in mutsu. Root cause: `dir_test_matches`
# (src/runtime/builtins_io.rs) called `.truthy()` on the return value of the
# `:test` predicate block, but a block whose sole/final statement is a bare
# regex (no `m`/`rx` prefix) returns the `Regex` object itself, not a
# `Match` -- Raku defers the actual match to `Regex.Bool`, evaluated
# against the topic in effect when the value is boolified, which
# `Value::truthy()` cannot see. Fixed by smart-matching the returned `Regex`
# against the entry name explicitly instead of boolifying it blindly.

plan 3;

mkdir 'tmp' unless 'tmp'.IO.d;
my $dir = 'tmp/bare-regex-routine-tail-implicit-match';
mkdir $dir unless $dir.IO.d;
spurt "$dir/keep.html", "x";
spurt "$dir/skip.txt", "x";

my @found = dir($dir, test => { /\.html$/ }).sort(*.basename);
is-deeply @found.map(*.basename).List, ('keep.html',),
    'dir(:test) with a bare-regex block excludes "." and non-matches';

my @entries = dir($dir, test => { /\.html$/ });
is @entries.elems, 1,
    'dir(:test) never returns the "." / ".." entries for a bare-regex predicate';

# A block predicate that plainly rejects everything must still reject
# everything -- i.e. the fix must not have made every entry match instead.
my @none = dir($dir, test => { /\.doesnotexist$/ });
is @none.elems, 0, 'dir(:test) with a never-matching bare-regex block returns nothing';

unlink "$dir/keep.html";
unlink "$dir/skip.txt";
rmdir $dir;
