use Test;

plan 6;

{
    my $a = 'z';
    is $a++, 'z', 'postfix increment returns old string value';
    is $a, 'aa', 'postfix increment carries string values';
}

{
    is state $i++, 0, 'postfix increment works on state declarator';
    is my $j.++, 0, 'dot postfix increment works on my declarator';
}

{
    my $a = '⁰';
    is (^12).map({ $a++ }), '⁰ ¹ ² ³ ⁴ ⁵ ⁶ ⁷ ⁸ ⁹ ¹⁰ ¹¹', 'superscript increment carries';

    my $b = '¹¹';
    is (^11).map({ --$b }), '¹⁰ ⁰⁹ ⁰⁸ ⁰⁷ ⁰⁶ ⁰⁵ ⁰⁴ ⁰³ ⁰² ⁰¹ ⁰⁰', 'superscript decrement borrows';
}
