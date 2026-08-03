unit module SlurpyLeak;

# A sigilless parameter keeps a module sub off the OTF-compiled call path, so it
# runs through the tree-walk fallback whose return merge writes the callee's
# `@`/`%` variables back into the caller under the same name. A *slurpy* one is
# never the caller's container -- the binder builds a fresh Array/Hash out of the
# leftover arguments -- so writing it back only clobbers a same-named caller
# lexical. `Test.rakumod`'s `throws-like(..., *%matcher)` hit exactly that when
# the code it ran called `fails-like(..., *%matcher)`.

sub run-block(&blk) { blk() }

sub inner-named(\code, $type, *%matcher) is export {
    die "inner saw: " ~ %matcher.keys.sort.join(',');
}

sub outer-named($code, *%matcher) is export {
    my $seen;
    run-block {
        CATCH { default { $seen = %matcher.keys.sort.join(','); } }
        $code();
    }
    $seen;
}

sub inner-positional(\code, $type, *@rest) is export {
    die "inner saw: " ~ @rest.join(',');
}

sub outer-positional($code, *@rest) is export {
    my $seen;
    run-block {
        CATCH { default { $seen = @rest.join(','); } }
        $code();
    }
    $seen;
}

# A non-slurpy `%h` parameter still writes back: it is the caller's own Hash.
sub mutate-hash(\code, %h) is export {
    %h<added> = 1;
}
