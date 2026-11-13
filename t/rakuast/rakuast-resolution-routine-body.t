use Test;
use experimental :rakuast;

# The declaration a call of a unit's sub resolves to answers its `body`, as
# rakudo's resolution (the declaration node itself) does: Red's compiler
# inlines a helper sub by reading `.resolution.body` (t/124, #17501).

plan 5;

my @seen;
sub above-one($value) { $value > 1 }
sub helper { (1, 2).grep({ above-one($_) }) }
sub recursive($n) { $n ?? recursive($n - 1) !! 0 }

CHECK {
    $*CU.visit: -> $n {
        if $n.^name eq 'RakuAST::Call::Name' {
            my $r = $n.resolution;
            @seen.push: $n.name.canonicalize => $r.body.statement-list.statements.elems
                if $r.^name eq 'RakuAST::Sub';
        }
        True
    }
}

my %seen = @seen;
is %seen<above-one>, 1, 'a call inside a closure reads its sub body';
is %seen<recursive>, 1, 'a recursive call reads it too';
is helper(), (2,), 'the subs still run';
is recursive(2), 0, 'and the recursive one';
ok @seen.elems >= 3, 'every call was seen';
