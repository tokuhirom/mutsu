use Test;

is infix:<X>($(2, 3), 6).gist, '(((2 3) 6))',
    'the routine form of X preserves an itemized List element';
is ([ $(2, 3) ] XX [6]).gist, '((((2 3) 6)))',
    'XX preserves an itemized List element through its X fold';
is ($(2, 3) X 6).gist, '(((2 3) 6))',
    'the operator form agrees with the routine form';
is infix:<X>([2, 3], 6).gist, '((2 6) (3 6))',
    'an ordinary Array operand still expands into elements';
is infix:<X>($(2, 3), 6).^name, 'Seq',
    'the routine form returns the same Seq type as the operator form';
is infix:<X>(1, 2, 3).gist, '((1 2 3))',
    'the routine form keeps the n-ary product as a tuple';

done-testing;
