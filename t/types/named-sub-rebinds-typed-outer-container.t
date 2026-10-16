use Test;

# A named sub's `:=` of an outer `@`/`%` variable that has a declared element
# type (`my Cool @a`) rebinds the outer variable itself: the variable takes a
# capture cell when the sub is registered, as an untyped one does, so the
# rebind reaches the declaring block without a frame's return merge carrying
# it (ADR-12529 §3, slice 32). Checked against rakudo.

plan 10;

my Int @src = 1, 2, 3;
my Cool @a;
sub bind-a() { @a := @src }
bind-a();
is @a.elems, 3, 'the typed outer array sees the bound container';
ok @a =:= @src, 'and is the same container';
is @a.of.^name, 'Int', 'it adopts the source element type';
throws-like { @a.push("x") }, X::TypeCheck, 'and enforces it';

my Int %hs = x => 1;
my Cool %h;
sub bind-h() { %h := %hs }
bind-h();
is %h<x>, 1, 'a typed outer hash sees the bound container';
is %h.of.^name, 'Int', 'and adopts its element type';

my Int @n = 7;
my Numeric @m;
sub bind-m() { @m := @n }
sub peek() { @m.join(',') }
bind-m();
@n.push(8);
is peek(), '7,8', 'another sub sees the rebind and later pushes';

my Str @q;
sub bad() { @q := @src }
throws-like { bad() }, X::TypeCheck::Binding, 'a mismatched element type is refused';
is @q.elems, 0, 'and leaves the variable alone';

my int @native = 1, 2;
sub bump() { @native[0] = 5 }
bump();
is @native[0], 5, 'a native array a sub writes keeps working';
