use Test;

plan 7;

is (my uint8 @a = 1000, 2000).gist, '[232 208]',
    'expression-position uint8 declaration wraps each initializer element';
is @a.gist, '[232 208]', 'the declared array keeps the wrapped elements';
is @a.of, uint8, 'the expression declaration retains its element type';
is @a.REPR, 'VMArray', 'the expression declaration uses native array storage';
is (my int8 @b = 200).gist, '[-56]',
    'signed native element width applies in expression position';
is @b.gist, '[-56]', 'the named signed array keeps the wrapped element';
is (my uint8 @c = 256).gist, '[0]',
    'a grouped expression-position declaration uses the same conversion';
