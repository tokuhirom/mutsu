use Test;

# In Raku the built-in `trait_mod:<is>` candidates live in the same multi as any
# user-declared one, so a user candidate that does not match simply does not
# consume the trait. mutsu routed every `is` trait through user multi-dispatch
# as soon as one existed, so an unrecognised trait came back as
# X::Multi::NoMatch instead of X::Comp::Trait::Unknown -- which merely importing
# `Test` was enough to trigger, since Test.rakumod exports
# `multi sub trait_mod:<is>(Routine:D $r, :$test-assertion!)`.

plan 6;

use MONKEY-SEE-NO-EVAL;

multi sub trait_mod:<is>(Routine:D $r, :$test-assertion!) { }

throws-like 'my $a is definitely-invalid = 5', X::Comp::Trait::Unknown,
    'an unknown variable trait is still X::Comp::Trait::Unknown';

throws-like 'my $a is readonly = 5', X::Comp::Trait::Unknown,
    'and so is an unsupported one';

# The built-in variable traits still work.
{
    my $a is default(41);
    is $a, 41, 'is default still applies';
}

lives-ok { EVAL 'my @b is List = 1, 2' }, 'is List still applies';

# The user candidate still claims the trait its signature names.
lives-ok { EVAL 'sub marked() is test-assertion { }' },
    'the user trait_mod:<is> still accepts its own trait';

# An error raised from INSIDE a matching handler is not swallowed by the
# fallback -- only the "no candidate matched" verdict is.
multi sub trait_mod:<is>(Variable:D $v, :$explodes!) { die "boom from handler" }
throws-like 'my $x is explodes = 1', X::AdHoc,
    'an error from inside a matching handler still propagates';
