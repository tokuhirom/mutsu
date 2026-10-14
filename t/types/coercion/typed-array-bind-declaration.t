use Test;

# `my Int @a := @src` binds a container whose element type conforms. The
# declaration's element-wise check must not test the bound Array itself
# against `Int`; the bind check compares the element types.

plan 6;

{
    my Int @c = 1;
    my Int @a := @c;
    @a.push(2);
    is @c.join(','), '1,2', 'a push through the bound name reaches the source';
    is @a.of.^name, 'Int', 'the bound array keeps its element type';
}

{
    my Int @c;
    my Int @a := @c;
    is @a.elems, 0, 'an empty typed source binds';
}

{
    my Int @c;
    my Cool @a := @c;
    pass 'a wider declared element type accepts the source';
}

throws-like { my Int @a := [1, 2] }, X::TypeCheck::Binding,
    'an untyped Array is still refused';
throws-like { my Str @c; my Int @a := @c }, X::TypeCheck::Binding,
    'a source of an unrelated element type is refused';
