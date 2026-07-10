use v6;
use Test;

# `die`ing a `but role` mixin of an Exception must preserve the exception type,
# so `CATCH { when X::Type }` still matches it (and an overridden `.message`
# dispatches through the mixin). Previously mutsu wrapped the mixin value in
# X::AdHoc, losing the type — `when` failed and the exception escaped. (zef
# find-prereq-candidates dies with `X::Zef::UnsatisfiableDependency.new but
# role { method message {...} }` and expects the alternative-spec CATCH to catch
# it.)

plan 6;

class X::Boom is Exception { method message { "base boom" } }

# 1. `when` on the base type catches a mixed-in exception.
my $caught-by-type = False;
{
    CATCH { when X::Boom { $caught-by-type = True; } }
    die X::Boom.new but role :: { method message { "mixed" } };
}
ok $caught-by-type, 'CATCH when BaseType catches a `but role` mixin exception';

# 2. The caught topic reports the real type, not X::AdHoc.
my $topic-type;
{
    CATCH { default { $topic-type = $_ ~~ X::Boom; } }
    die X::Boom.new but role :: { method x { 1 } };
}
ok $topic-type, 'caught topic smartmatches the base exception type';

# 3. An overridden `.message` on the mixin is honored.
my $msg;
{
    CATCH { when X::Boom { $msg = $_.message; } }
    die X::Boom.new but role :: { method message { "overridden" } };
}
is $msg, 'overridden', 'overridden .message dispatches through the mixin';

# 4. throws-like sees the mixed-in exception as its base type.
throws-like { die X::Boom.new but role :: { method message { "m" } } },
    X::Boom, 'throws-like matches a `but role` mixin exception';

# 5. A plain (non-mixin) exception still works (no regression).
my $plain = False;
{
    CATCH { when X::Boom { $plain = True; } }
    die X::Boom.new;
}
ok $plain, 'plain exception still caught by type';

# 6. A `but role` mixin of a NON-exception still stringifies via X::AdHoc.
class Plain { method Str { "plain-str" } }
my $adhoc-msg;
{
    CATCH { default { $adhoc-msg = $_.message; } }
    die Plain.new but role :: { method Str { "mixed-str" } };
}
is $adhoc-msg, 'mixed-str', 'non-exception mixin dies as X::AdHoc with its Str';
