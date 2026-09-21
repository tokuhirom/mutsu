use Test;

# From Air::Functional (ecosystem parity, via Air::Plugin::Donate): every
# rendered tag comes back through `--> Markup()`, where
# `role Markup is Str is export(:MANDATORY) {}`. Such a role says "a Str that
# also does Markup", which is exactly what mutsu's mixin wrapper represents --
# a non-Instance value has no shared attribute node to rebless, so the role
# rides in the wrapper (see src/runtime/types/role_mixin_class.rs). The role
# offered no CALL-ME/COERCE/new, so the coercion died with
# X::Coerce::Impossible instead.
#
# The CLASS twin (`class C is Str {}; C('x')`) is covered separately in
# t/oo/class/coerce-into-class-inheriting-builtin.t (see
# src/runtime/types/native_backed_class.rs).

plan 10;

role Markup is Str { }

# The call spelling.
my $m = Markup('hello');
is $m.Str, 'hello', 'coercing a Str into a role that inherits Str keeps the string';
ok $m ~~ Markup, '... and the result does the role';
ok $m ~~ Str, '... and is still a Str';
is "interp $m", 'interp hello', '... and interpolates as the string';

# The return-type spelling, which is how Air writes it.
sub mk(--> Markup()) { 'yo' }
is mk().Str, 'yo', 'a `--> Role()` return type coerces the same way';
ok mk() ~~ Markup, '... and the returned value does the role';

# A non-Str built-in parent, to show the rule is not Str-specific.
role Counted is Int { }
my $c = Counted(42);
is $c + 1, 43, 'a role inheriting Int coerces an Int and stays numeric';
ok $c ~~ Counted, '... and does the role';

# A role with NO built-in parent is untouched: still an impossible coercion.
role Plain { }
dies-ok { Plain('x') },
    'a role with no built-in parent still refuses to coerce';

# Multi dispatch must prefer the role over its own built-in parent, or a
# framework that renders through `multi render-tag(Markup $)` and escapes
# through `multi render-tag(Str() $)` escapes its own markup.
multi sub render(Markup $i) { "raw($i)" }
multi sub render(Str()   $i) { "escaped($i)" }
is render(Markup('t')), 'raw(t)',
    'a role that inherits a type is narrower than that type for multi dispatch';
