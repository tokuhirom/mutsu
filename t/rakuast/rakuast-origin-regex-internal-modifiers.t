use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a regex's internal modifier
# (`:i`, `:!ignorecase`, `:s`, `:m`, `:r`) is placed over its colon, its `!`
# and its short or long name, and the regex atoms after it are placed too.

plan 5;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '').subst('Regex::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) if $n.^name.starts-with('RakuAST::Regex') }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[/ :i a /]),
    'Sequence@2-7 InternalModifier::IgnoreCase@2-4 WithWhitespace@5-7 Literal@5-6',
    'a short modifier';
is placed(Q[/ :!ignorecase a /]),
    'Sequence@2-17 InternalModifier::IgnoreCase@2-14 WithWhitespace@15-17 Literal@15-16',
    'a negated long modifier';
is placed(Q[/ a :s b c /]),
    'Sequence@2-11 WithWhitespace@2-4 Literal@2-3 InternalModifier::Sigspace@4-6 WithWhitespace@7-9 Literal@7-8 WithWhitespace@9-11 Literal@9-10',
    'a modifier inside a sequence';
is placed(Q[/ :m :r a /]),
    'Sequence@2-10 InternalModifier::IgnoreMark@2-4 InternalModifier::Ratchet@5-7 WithWhitespace@8-10 Literal@8-9',
    'two modifiers';
is placed(Q[my $x; / :i $x /]),
    'Sequence@9-15 InternalModifier::IgnoreCase@9-11 WithWhitespace@12-15 Interpolation@12-14',
    'a modifier before an interpolation';
