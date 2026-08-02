use lib $?FILE.IO.parent.add('lib').Str;
use Test;
use PackageSubScope;

# A plain `sub` in a module body is `my`-scoped. It is exported and callable
# under its short name, but it is NOT a package symbol: raku answers
#
#     Could not find symbol '&lex-sub' in 'PackageSubScope'
#
# for `PackageSubScope::lex-sub()`. Only `our sub` is reachable that way.
#
# mutsu leaked both shapes. A single `sub` leaked through the qualified-call
# retry, which strips a package prefix and calls the short name -- that retry
# exists for a package mutsu never registered, and must not fire for one it
# knows. A `multi sub` leaked one level deeper: multis are registered under
# `Pkg::name/arity` keys, so the exact-name lookup that carried the
# my-scoped gate missed, and the arity-keyed candidate scan below it handed
# back the routine the gate had just refused.

plan 8;

is lex-sub(), 'lex', 'a lexical module sub is importable';
is multi-lex(1), 'multi-lex 1', 'and so is a lexical multi';
is pkg-sub(), 'pkg', 'an our sub is importable too';

throws-like { PackageSubScope::lex-sub() }, X::AdHoc,
    message => /"Could not find symbol '&lex-sub' in 'PackageSubScope'"/,
    'a lexical module sub is not a package symbol';

throws-like { PackageSubScope::multi-lex(1) }, X::AdHoc,
    message => /"Could not find symbol '&multi-lex' in 'PackageSubScope'"/,
    'nor is a lexical multi, whose candidates are arity-keyed';

is PackageSubScope::pkg-sub(), 'pkg', 'an our sub is a package symbol';
is PackageSubScope::multi-pkg(1), 'multi-pkg 1', 'and so is an our multi';

throws-like { PackageSubScope::inside-qualified-lex() }, X::AdHoc,
    message => /"Could not find symbol '&lex-sub' in 'PackageSubScope'"/,
    'not even from inside the package -- a lexical is never in the stash';
