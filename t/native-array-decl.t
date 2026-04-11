use Test;

plan 8;

{
    my int @a;
    ok @a ~~ array[int], 'my int @a smartmatches native array[int]';
    is @a.WHAT, '(array[int])', 'my int @a WHAT keeps native array type';
    is @a.raku, 'array[int].new()', 'my int @a raku keeps native array type';
}

{
    my @a of num;
    ok @a ~~ array[num], 'my @a of num smartmatches native array[num]';
    is @a.WHAT, '(array[num])', 'my @a of num WHAT keeps native array type';
}

{
    my @a := array[str].new('a', 'b');
    ok @a ~~ array[str], 'array[str].new smartmatches native array[str]';
    is @a.WHAT, '(array[str])', 'array[str].new WHAT keeps native array type';
    is @a.raku, "array[str].new(\"a\", \"b\")", 'array[str].new raku keeps native array type';
}
