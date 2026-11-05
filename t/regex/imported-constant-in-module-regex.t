use v6;
use Test;
use lib 't/lib';
use RegexConstImporter;

# A `constant` a module imported with `use` must be interpolated in a regex
# written inside that module's own routines. It lives in the module's unit
# pad, not in the calling frame's env, so the interpolation used to read it as
# an undefined value: an empty pattern, which in a `s:g[ ... ]` rewrote the
# wrong characters (seen in the CSS::Minifier dist, where `!` became `"`).

plan 8;

is subst-it('xZZ! y'), 'x"! y', 's:g with an imported constant rewrites only the constant';
is subst-it('a!b'), 'a!b', 'no occurrence: the subject is unchanged';
is match-all('ZZ-ZZ!'), 2, 'm:g with an imported constant finds each occurrence';
is match-all('!!!'), 0, 'm:g finds nothing when the constant is absent';
ok match-one('xZZ'), 'm// with an imported constant matches';
nok match-one('xYY'), 'm// with an imported constant does not match another string';
is private-use('"x! y"'), '"x! y"', 'a private-use-area constant leaves other characters alone';
is Holder.new.subst-it('xZZ! y'), 'x"! y', 'the same holds in a class method';
