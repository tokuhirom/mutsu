# Fixture for t/modules/module-check-cu-rewrite.t: CHECK-time code that walks
# the calling unit's `$*CU`, as Red's `red-compile` does, and rewrites every
# `.grep(...)` call to pass a `:tag(42)` named argument.
unit module CheckTagGrep;

# The `compile-time-value`s of the calls to `&above` the walk saw.
our @seen-above;

sub tag-greps($cu = $*CU) is export {
    sub walk($n) {
        $n.visit-children(&walk);
        if $n.^name eq 'RakuAST::ApplyPostfix'
            && $n.postfix.^name eq 'RakuAST::Call::Method'
            && $n.postfix.name.canonicalize eq 'grep'
        {
            $n.postfix.args.push(
                RakuAST::ColonPair::Value.new(:key<tag>, :value(RakuAST::IntLiteral.new(42))));
        }
        if $n.^name eq 'RakuAST::Call::Name' && $n.name.canonicalize eq 'above'
            && $n.is-resolved
        {
            @seen-above.push: $n.resolution.compile-time-value;
        }
    }
    walk($cu);
}
