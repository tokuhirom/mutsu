use v6;
use lib 't/lib';
use Test;

plan 1;

# #8083: a role method whose parameter names a GENUINELY mistyped type is
# silently accepted at role-registration time whenever the role body has a
# `use` of a module that has not loaded yet -- the not-yet-loaded module
# MIGHT have supplied the type, so `role_body_method_decl` defers the check
# optimistically. But nothing ever re-validates the deferred check once the
# module actually loads, so a truly bogus name is accepted forever and the
# method that names it silently disappears once the role is composed,
# instead of raising X::Parameter::InvalidType the way an immediately
# unresolvable name already does.

use RolePendingTypo::R;

throws-like
    { class Consumer does RolePendingTypo::R { } },
    X::Parameter::InvalidType,
    'a genuinely mistyped role-method param type is still caught once the role body\'s use has run';
