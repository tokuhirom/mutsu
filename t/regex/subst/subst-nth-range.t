use Test;

# `.subst(..., :nth(2..*))` selects "every match from the 2nd on". mutsu used to
# expand the Range eagerly into a Vec of indices, so an infinite upper bound
# asked for an `i64::MAX`-element allocation and the process aborted with a Rust
# `capacity overflow` panic. Range arguments now defer to the same resolution
# `.match(:nth(...))` uses, which clamps every Range flavour to the number of
# matches actually found. Reduced from `Template6`'s `Parser.compile`, which
# ends in `$script.subst(/ 'my %localdata;' /, '', :nd(2..*))`.

plan 16;

my $s = "x x x x x";

is $s.subst(/x/, 'Y', :nth(2..*)),  "x Y Y Y Y", ':nth with an infinite Range';
is $s.subst(/x/, 'Y', :nd(2..*)),   "x Y Y Y Y", ':nd ordinal alias with an infinite Range';
is $s.subst(/x/, 'Y', :st(1..*)),   "Y Y Y Y Y", ':st ordinal alias with an infinite Range';
is $s.subst(/x/, 'Y', :nth(2..3)),  "x Y Y x x", ':nth with a bounded Range';
is $s.subst(/x/, 'Y', :nth(2..^4)), "x Y Y x x", ':nth with an end-exclusive Range';
is $s.subst(/x/, 'Y', :nth(1^..3)), "x Y Y x x", ':nth with a start-exclusive Range';
is $s.subst(/x/, 'Y', :nth(1^..^4)),"x Y Y x x", ':nth with a both-exclusive Range';

# A Range that runs past the end of the match list is clamped, not an error.
is $s.subst(/x/, 'Y', :nth(4..99)), "x x x Y Y", ':nth Range past the last match is clamped';
is $s.subst(/x/, 'Y', :nth(9..*)),  "x x x x x", ':nth Range starting past the last match matches nothing';

# The non-Range forms are unaffected.
is $s.subst(/x/, 'Y', :nth(*)),     "x x x x Y", ':nth(*) still means the last match';
is $s.subst(/x/, 'Y', :nth(*-1)),   "x x x Y x", ':nth(*-1) still counts back from the end';
is $s.subst(/x/, 'Y', :nth(2)),     "x Y x x x", ':nth with a plain Int';
is $s.subst(/x/, 'Y', :nth(1, 3)),  "Y x Y x x", ':nth with a list of Ints';

# Ranges work over a longer subject and with a Callable replacement too.
my $t = "a1b2c3d4";
is $t.subst(/\d/, '#', :nth(2..*)), "a1b#c#d#", 'Range :nth over a longer subject';
is $t.subst(/(\d)/, { $0 * 2 }, :nth(2..*)), "a1b4c6d8", 'Range :nth with a Callable replacement';

# A Range :nth on a string pattern goes through the same path.
is "ababab".subst('a', 'Z', :nth(2..*)), "abZbZb", 'Range :nth with a string pattern';
