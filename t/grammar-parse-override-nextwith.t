use v6;
use Test;

# A grammar that overrides `parse` and re-dispatches to the native grammar
# parse via `nextwith`, injecting an actions object — the pattern used by
# YAMLish's `method parse { nextwith($input, :actions(Actions)) }`. Before the
# fix, `nextwith` from an overridden grammar `parse`/`subparse` had no MRO
# candidate to defer to (the native parse is not a MethodDef), so it returned an
# undefined match and every parse failed.

plan 6;

grammar Simple {
    token TOP { <digits> }
    token digits { \d+ }

    class Actions {
        method TOP($/) { make $<digits>.Str.Int }
        method digits($/) { make $/.Str.Int }
    }
    method parse($input, *%args) {
        nextwith($input, :actions(Actions), |%args);
    }
    method subparse($input, *%args) {
        nextwith($input, :actions(Actions), |%args);
    }
}

my $m = Simple.parse("42");
ok $m.defined, 'overridden parse + nextwith matches';
is ~$m, '42', 'match stringifies to the whole input';
is $m.ast, 42, 'the injected actions ran (ast is made value)';

my $s = Simple.subparse("99abc");
ok $s.defined, 'overridden subparse + nextwith matches a prefix';
is ~$s, '99', 'subparse consumed the leading digits';

# An explicit :actions passed by the caller still flows through |%args.
grammar Plain {
    token TOP { \w+ }
    method parse($input, *%args) { nextwith($input, |%args) }
}
ok Plain.parse("hello").defined, 'nextwith without an actions object still parses';
