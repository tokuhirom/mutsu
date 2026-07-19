use Test;

# Chained bind whose middle target is an angle-word hash subscript:
#   my $x := %h<key> := VALUE
# The `<key>` postcircumfix is word-quoting (a literal string key), not an
# expression, so `%h<key> := ...` must parse it as a word. mutsu previously
# parsed the inside of `<...>` as an expression, mis-reading `%h<b> := 5` as a
# `b > ...` comparison, so any chained bind through an angle subscript failed to
# parse. Found via the real-distribution compat sweep (Ecosystem::Archive,
# docs/dist-compat-sweep.md), which uses `my $id := %distribution<dist> := ...`.

plan 8;

# The Ecosystem::Archive idiom: bind a scalar to an angle-subscript bind
{
    my %d = dist => 'old';
    my $id := %d<dist> := 'new';
    is $id, 'new', 'scalar bound to angle-subscript bind sees the bound value';
    is %d<dist>, 'new', 'the angle subscript was bound';
}

# Chained assignment (=) through an angle subscript still works
{
    my %h;
    my $x := %h<a> = 9;
    is %h<a>, 9, 'angle subscript assignment in a bind chain';
    is $x, 9, 'the bind sees the assigned value';
}

# Multi-word angle subscript bind
{
    my %h;
    %h<a b> := (1, 2);
    is %h<a>, 1, 'multi-word angle bind, first key';
    is %h<b>, 2, 'multi-word angle bind, second key';
}

# A real comparison `<` (not a subscript) is unaffected
{
    my $x = 3;
    ok $x < 5, 'bare `<` still parses as numeric comparison';
    my %h = a => 1;
    ok %h<a> < 5, 'angle subscript followed by a comparison still works';
}
