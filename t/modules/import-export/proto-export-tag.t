use v6;
use lib 't/lib';
use Test;

# Regression (FunctionalParsers 0.1.10, tokuhirom/mutsu#8526): a `proto NAME(|)
# is export(:TAG, :ALL)` with `multi` candidates that carry no `is export` of
# their own must export the whole multi family under TAG, not just DEFAULT.
#
# `Stmt::ProtoDecl` had no `export_tags` field at all, so
# `exec_register_proto_sub_op` always called `register_exported_sub` with an
# empty tag list, which defaults to `["DEFAULT"]` — silently dropping every
# other tag the proto declared. `use Mod :mine` then saw `combine`'s
# `symbol_tags == {"DEFAULT"}`, found it disjoint from the requested
# `{"mine"}`, and skipped importing it entirely, leaving the proto with zero
# candidates: "Cannot resolve caller combine(...); none of these signatures
# matches".

plan 2;

{
    my $out = EVAL q:to/CODE/;
        use ProtoExportTagFixture :mine;
        combine(1, 2)
    CODE
    is $out, 3, ':mine-tagged use imports the proto AND its multi candidates';
}

{
    my $died = False;
    try {
        EVAL q:to/CODE/;
            use ProtoExportTagFixture :mine;
            combine('a')
        CODE
        CATCH { default { $died = True } }
    }
    ok !$died, 'the single-arg candidate is reachable too, not just the arity that ran first';
}
