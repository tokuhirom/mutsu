use Test;

# `throws-like 'CODE', ...` runs the string CODE in the caller's lexical scope
# (Raku EVALs it there). Mutations the code makes to a caller variable before it
# throws must be visible afterwards — but a fresh `my` inside the code must NOT
# leak back to a same-named caller variable.

plan 6;

# A loop that builds a string for the complete groups, then dies on the last
# incomplete one. The partial work must survive.
my $str = '';
throws-like 'for 1..5 ->  $x, $y { $str ~= "$x$y" }', Exception,
    'partial loop work runs before the throw';
is $str, "1234", 'caller $str saw the writeback from the thrown code';

# A plain assignment (no `my`) before a die writes back.
my $acc = 0;
throws-like 'for 1..3 { $acc += $_ }; die "boom"', Exception, 'dies after accumulating';
is $acc, 6, 'caller $acc accumulated via writeback';

# A `my` inside the code shadows: the caller variable is untouched.
my $shadowed = 10;
throws-like 'my $shadowed = 999; die "x"', Exception, 'dies with a shadowing my';
is $shadowed, 10, 'a fresh my inside throws-like does not clobber the caller';
