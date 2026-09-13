use Test;

# Runtime interpolation changes the concrete regex text, but repeating the
# same value should reuse the parsed top-level pattern. A later value must get
# its own entry rather than reusing the old tree.
plan 4;

my $needle = 'a';
ok 'a' ~~ /$needle/, 'the first interpolated pattern matches';
ok 'a' ~~ /$needle/, 'the same interpolated pattern matches again';

$needle = 'b';
ok 'b' ~~ /$needle/, 'a changed interpolated value gets a matching pattern';
ok 'a' !~~ /$needle/, 'a changed interpolated value does not use the old pattern';
