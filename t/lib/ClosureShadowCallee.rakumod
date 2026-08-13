unit module ClosureShadowCallee;
use Test;

# Exported (not locally declared) so a caller's bareword call to it compiles
# through the `Stmt::Call` path rather than `Expr::Call`/`Expr::UserRoutineCall`
# -- see news/2026-08/closure-capture-shadowed-by-colliding-callee-parameter.md.
# That path is what the fix targets. The `.tap`/`sleep` machinery isn't
# incidental scaffolding: it is part of what made the original bug reproduce
# (a leaner version without it did not trip the bad merge path).
sub closure-shadow-callee($s, $expected, :&after) is export {
    subtest {
        plan 3;
        ok $s ~~ Supply, "is supply";
        my @res;
        my $done;
        $s.tap({ @res.push($_) }, :done({ $done = True }));
        after() if &after;
        for ^50 { last if $done; sleep .1 }
        ok $done, "done";
        is-deeply @res, $expected, "results";
    }, "inner";
}
