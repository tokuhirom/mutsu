use v6;

# Fixture for t/tail-stmt-call-named-value.t: a module sub whose body ends in
# a statement-position call with named arguments (JSON::Marshal's `marshal`
# ends in `to-json($ret, :$sorted-keys, :$pretty)`).
module TailNamedCall {
    use JSON::Fast;

    sub render(Any $obj, Bool :$pretty = True --> Str) is export {
        my $ret = $obj;
        to-json($ret, :$pretty);
    }
}
