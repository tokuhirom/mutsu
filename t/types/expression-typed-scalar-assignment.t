use Test;

plan 5;

{
    is (my Int:D $x = 1), 1, 'expression declaration returns its initializer';
    throws-like { $x = Nil }, X::TypeCheck::Assignment,
        'a later Nil assignment keeps the expression declaration constraint';
}

{
    my Int:D $x = 1;
    throws-like { $x = Nil }, X::TypeCheck::Assignment,
        'statement declaration still rejects Nil';
}

{
    is (my Int $x = 1), 1, 'plain typed expression declaration returns its value';
    $x = Nil;
    ok $x === Int, 'a plain typed scalar still resets Nil to its type object';
}
