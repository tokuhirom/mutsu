use Test;

# A role body captures the class's own `AT-KEY` with `::?CLASS.^find_method`
# and wraps it; for `class H is Hash` that method is Hash's, so calling it on
# an H must not find the role's wrapper again.
plan 2;

role Wrap {
    my &AT-KEY := ::?CLASS.^find_method('AT-KEY');
    method AT-KEY(::?CLASS:D: $key is raw) is raw {
        "wrapped(" ~ AT-KEY(self, $key) ~ ")"
    }
}

class Base { method AT-KEY($k) { "base($k)" } }
class OverBase is Base does Wrap { }
is OverBase.new.AT-KEY("x"), "wrapped(base(x))", 'a user parent still works';

class OverHash is Hash does Wrap { }
my $h = OverHash.new;
$h.BIND-KEY("a", 1);
is $h.AT-KEY("a"), "wrapped(1)", 'the role wraps the Hash method';
