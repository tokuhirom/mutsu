#!/usr/bin/env raku
# ADR-0029 Slice 2: probe the real shape of a list of `X::` (or any) type
# names -- one process, one pass, run identically under both `raku` (the
# oracle) and `mutsu` (to diff against).
#
# Reads one name per line from STDIN (e.g. "X::Comp::AdHoc"). For each name
# that resolves to a real Exception subtype, prints a TSV row:
#   name  found  mro          roles_direct   roles_all
# `mro`/`roles_direct`/`roles_all` are comma-joined `.^name` lists in the
# order the metamodel returns them. A name that does not resolve (mutsu has
# not registered it yet, or the name is test-local, e.g. a roast-only
# `X::Boom`) prints only `name  0` -- callers must not guess at its shape.
#
# Usage: raku scripts/probe-x-exception-shape.raku < names.txt > out.tsv
#        mutsu scripts/probe-x-exception-shape.raku < names.txt > out.tsv

for lines() -> $name {
    next unless $name.chars;
    my $type = ::($name);
    # Type objects are legitimately undefined -- do NOT gate on `.defined`.
    # An unresolved symbol comes back as a `Failure`, which does not smartmatch
    # `Exception` (it is not itself an exception type), so this one check
    # covers both "unknown name" and "resolves to something that isn't an
    # Exception subtype".
    # A role that composes into Exception subtypes (e.g. `X::Comp` itself)
    # smartmatches `Exception` too, but has no `.^mro` -- only classes are
    # `register_x` candidates, so treat a role-shaped name (anything not a
    # plain ClassHOW) as not-found here.
    if !($type ~~ Exception) || !($type.HOW ~~ Metamodel::ClassHOW) {
        say "$name\t0";
        next;
    }
    my @mro = $type.^mro.map(*.^name);
    # mutsu's `::($name)` dynamic lookup auto-vivifies a phantom stub class
    # for a name it has never registered (mro `(name Any Mu)`, no Exception
    # in it -- a separate mutsu bug, see
    # todo/tickets/dynamic-package-lookup-autovivifies-unknown-class.md).
    # A genuinely-registered `X::` exception class's mro always contains
    # `Exception` (`register_x` guarantees it), so this also doubles as the
    # "not really found" signal for that stub.
    if $name ne 'Exception' && !@mro.grep('Exception') {
        say "$name\t0";
        next;
    }
    my @roles-direct = $type.^roles(:!transitive).map(*.^name);
    my @roles-all = $type.^roles.map(*.^name);
    say "$name\t1\t{@mro.join(',')}\t{@roles-direct.join(',')}\t{@roles-all.join(',')}";
}
