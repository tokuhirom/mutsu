use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a named colonpair ending a
# routine's name (`method comment:sym<#>`) is a `ColonPair::Value` over
# `:sym<#>`, its `QuotedString` over `<#>` and its `StrLiteral` over the text.

plan 3;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) if $class ~~ /Name|ColonPair|Quoted|StrLiteral/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[class C { method comment:sym<#>($/) { } }]),
    'Name@6-7 Name@17-31 ColonPair::Value@24-31 QuotedString@28-31 StrLiteral@29-30',
    'a sym colonpair';
is placed(Q[class D { method m:sym<a b>() { } }]),
    'Name@6-7 Name@17-27 ColonPair::Value@18-27 QuotedString@22-27 StrLiteral@23-26',
    'a sym colonpair with a space';
is placed(Q[sub infix:<foo>($a, $b) { }]),
    'Name@4-15 QuotedString@10-15 StrLiteral@11-14',
    'an operator name';
