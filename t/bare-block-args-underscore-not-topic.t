use Test;

plan 4;

# `call_compiled_closure_with_topic` (src/vm/vm_closure_dispatch.rs) used to
# bind the implicit topic $_ to the first positional argument of a bare
# block whenever the block declared no explicit signature/placeholder
# params -- regardless of whether the block's body actually reads @_
# instead of $_. The tree-walk closure-call branch (call_sub_value in
# src/runtime/resolution_call_sub.rs) already got this right by scanning the
# body with auto_signature_uses() and skipping the $_ bind when it finds a
# bare @_ read; call_compiled_closure_with_topic now does the same scan.
#
# Raku's actual rule (verified against `raku` itself): a bare block whose
# body reads @_ anywhere never gets $_ auto-bound, and accepts any number of
# positional args; a bare block that reads only $_ gets $_ bound to the
# first arg, with arity restricted to 0 or 1 (extra args are a compile/
# runtime arity error).

# A bare block reading @_ must NOT have $_ bound to the first argument, and
# accepts any number of args.
{
    my @seen;
    { @seen.push("$_.defined()/@_[]") }.(1, 2, 3);
    is @seen[0], 'False/1 2 3', 'bare block reading @_ leaves $_ unbound (direct .() call)';
}

# The same shape invoked with no args at all still inherits the caller's
# topic (unaffected code path, sanity check).
{
    my @seen;
    for 'outer-topic' {
        { @seen.push($_) }.();
    }
    is @seen[0], 'outer-topic', 'bare block with no args still inherits caller topic';
}

# A bare block that reads $_ (not @_) keeps binding $_ to the first arg.
{
    my @seen;
    { @seen.push($_) }.(42);
    is @seen[0], 42, 'bare block reading $_ still binds it to the first positional arg';
}

# A block reading both $_ and @_ leaves $_ unbound too -- @_ being read
# anywhere in the body suppresses the implicit $_ bind regardless of
# whether $_ is also read.
{
    my @out;
    { @out.push($_.defined); @out.push(@_.join(',')) }.(7, 8);
    is @out.join('|'), 'False|7,8', 'bare block reading both $_ and @_ leaves $_ unbound too';
}
