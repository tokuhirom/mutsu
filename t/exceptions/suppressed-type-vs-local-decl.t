use Test;

plan 4;

# A module class's `my`-scoped nested type (e.g. `my grammar Header` inside
# Cro::HTTP::Header) is suppressed outside its parent — but it must NOT make
# a same-named LOCAL declaration unresolvable in consumer code, and inside
# the parent the lexical type must still win over any caller-scope value.
# (Cro::HTTP::RequestParser declares `my enum Expecting <RequestLine Header
# Body>` while Cro::HTTP::Header has a lexical `my grammar Header`.)

use lib $*PROGRAM.parent.add('suppressed-type-vs-local-decl-lib').Str;
use SuppMod;

my enum E <A Header B>;
is Header.Int, 1, 'local enum value resolves despite suppressed module-lexical type';
ok Header ~~ E, 'the resolved value is the enum value';
is SuppMod::Thing.check("abc"), True, 'inside the parent class the lexical grammar still wins';
is SuppMod::Thing.check("!!!"), False, 'grammar actually matches (not the enum leaking in)';
