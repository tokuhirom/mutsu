use Test;

# The nodes of a whatever-curried infix (`* + 1`, `3 < *`) and of a call on a
# term (`(* + 1)(2)`) carry the ranges Rakudo gives them (ADR-10723 Stage 2,
# #15780): the priming `*` has no origin of its own, yet the `ApplyInfix`,
# its `Infix` and its `ArgList` span it, and a `Call::Term` runs from its `(`
# to its `)` with the `ArgList` inside. Ranges measured on rakudo 2026.09.

plan 4;

# Every visited node of `$src` with its range, as `Class from..to` words.
sub ranges(Str $src) {
    my @r;
    sub walk($node) {
        my $name = $node.^name.subst('RakuAST::', '');
        @r.push: $name ~ ' ' ~ ($node.origin andthen "{.from}..{.to}" orelse '-');
        $node.visit-children(&walk);
    }
    $src.AST.visit-children(&walk);
    @r
}

sub has(@got, Str $want) { so @got.first($want) }

my @left = ranges('my $f = * + 1;');
ok has(@left, 'ApplyInfix 8..13'), 'ApplyInfix spans a left whatever';
ok has(@left, 'Infix 10..11') && has(@left, 'ArgList 8..13'), 'its Infix and ArgList';

my @right = ranges('my @b = (1, 2).grep(3 < *);');
ok has(@right, 'ApplyInfix 20..25') && has(@right, 'Infix 22..23'), 'a right whatever';

my @call = ranges('my @a = (1, 2); (* + 1)(2);');
ok has(@call, 'Call::Term 23..26') && has(@call, 'ArgList 24..25'), 'a call on a term';
