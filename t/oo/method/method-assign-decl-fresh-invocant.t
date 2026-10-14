use Test;

plan 12;

# An untyped `my $x .= meth` calls the method on the freshly declared
# variable, never on a same-named variable of an outer scope (#13273).

my $q = "x";
{
    my $q .= Str;
    is $q, '', 'the invocant is the fresh $q, not the outer "x"';
}
is $q, 'x', 'the outer $q is untouched';

my $bang = method { self.defined ?? 'def' !! 'undef' };
{
    my $q .= $bang;
    is $q, 'undef', 'an indirect method sees the fresh, undefined $q';
}

my @a = 1, 2;
{
    my @a .= push(3);
    is @a.raku, '[3]', 'my @a .= push starts from an empty array';
}
is @a.raku, '[1, 2]', 'the outer @a is not pushed onto';

my %h = a => 1;
{
    my %h .= push((b => 2));
    is %h.raku, '{:b(2)}', 'my %h .= push starts from an empty hash';
}
is %h.keys.sort.join(','), 'a', 'the outer %h is unchanged';

{
    my $z is default(5) .= succ;
    is $z, 6, 'the invocant is the declared default value';
}

sub inner-shadow() {
    my $w = 10;
    { my $w .= succ; $w }
}
throws-like { inner-shadow() }, X::Method::NotFound,
    'inside a routine, the fresh $w is Any, not the outer 10';

{
    my $n .= new;
    is $n.^name, 'Any', 'Any.new for an untyped declaration';
}

{
    my $s = 0;
    for ^3 { state $c .= new; $s++ }
    is $s, 3, 'a state .= declaration still runs once per loop';
}

my $top .= Str;
is $top, '', 'a file-scope .= declaration';
