use lib 't/lib';
use Test;
use RoleButInitLexicalRole;

# `$x but R(v)` inside a module sub, where `R` is the module's own file-scoped
# `my role`, must apply the role with `v` as its initializer instead of calling
# `R(v)` as a coercion (seen in the P5-X distribution).
plan 3;

my $r = tag-it("abc");
is $r.subject, "abc", 'role initializer reaches the module-level lexical role';
is $r + 1, 2, 'the mixed-in value keeps its numeric value';
is tag-it(IO::Path.new("foo")).subject, "foo".IO, 'works with a non-Str initializer';
