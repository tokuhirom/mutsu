use Test;

plan 14;

# A named parameter spelled as an alias chain (`:r(:@regex)`) declares the
# variable in its innermost link. Introspection reports that variable's
# sigil, name and type, and every link as a named name — Getopt::Long builds
# its option table from exactly these (`$param.named_names`, `.sigil`,
# `.type.of`), so `multi MAIN(:r(:@regex))` was rejected as "Invalid name(s)".

my &s = sub (:r(:@regex), Int :n(:@nums), :h(:%hh), Bool :s(:$start-service),
             :x($a), :z(:w(:v($b)))) { };
my @p = &s.signature.params;

is-deeply @p[0].named_names.List, ('regex', 'r'), ':r(:@regex) named_names';
is @p[0].sigil, '@', ':r(:@regex) sigil';
is @p[0].usage-name, 'regex', ':r(:@regex) usage-name';
ok @p[0].type =:= Positional, ':r(:@regex) type is Positional';
is @p[1].type.^name, 'Positional[Int]', 'a typed alias array keeps its element type';
is @p[2].sigil, '%', ':h(:%hh) sigil';
ok @p[2].type =:= Associative, ':h(:%hh) type is Associative';
is-deeply @p[3].named_names.List, ('start-service', 's'), 'Bool :s(:$start-service) named_names';
is @p[3].sigil, '$', 'a scalar alias chain keeps the $ sigil';
is-deeply @p[4].named_names.List, ('x',), ':x($a) names only the named link';
is @p[4].usage-name, 'a', ':x($a) usage-name is the variable';
is-deeply @p[5].named_names.List, ('v', 'w', 'z'), 'a three-link chain lists innermost first';

# An unparameterized container or code type object has no element constraint.
is-deeply (Positional, Associative, Callable, Code, List, Map).map(*.of).List,
    (Mu, Mu, Mu, Mu, Mu, Mu), '.of on an unparameterized role/type object is Mu';
ok Positional[Int].of =:= Int, '.of on a parameterized role still reports its argument';
