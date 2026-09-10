use Test;

plan 5;

# `our $.attr` / `our @.attr` / `our %.attr` inside a role body is rejected
# at compile time in raku (X::Declaration::OurScopeInRole, "the scope inside
# of a role is generic, so there is no unambiguous package to install the
# symbol in") -- the same restriction `role_body_our_scope_violation`
# (src/opcode.rs) already enforces for `our sub`/`our class`/`our role`, just
# not for an our-scoped attribute declaration. `my $.attr` and a plain
# `has $.attr` are unaffected -- only the `our`-scoped form is forbidden.

throws-like 'role R { our $.shared = "x"; }', X::Declaration::OurScopeInRole,
    'our-scoped scalar attribute inside a role is forbidden';

throws-like 'role R { our @.shared; }', X::Declaration::OurScopeInRole,
    'our-scoped array attribute inside a role is forbidden';

throws-like 'role R { our %.shared; }', X::Declaration::OurScopeInRole,
    'our-scoped hash attribute inside a role is forbidden';

# `my $.attr` (lexically scoped, not package-scoped) stays legal.
role MyScoped { my $.shared = "my-attr"; }
class UsesMyScoped does MyScoped {}
is UsesMyScoped.shared, 'my-attr', 'my-scoped attribute inside a role still works';

# A plain (per-instance) `has $.attr` stays legal.
role PlainAttr { has $.x; }
class UsesPlainAttr does PlainAttr {}
my $obj = UsesPlainAttr.new(x => 42);
is $obj.x, 42, 'plain has-attribute inside a role still works';
