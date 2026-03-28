use Test;
plan 11;

# Role with `is rw` makes public attributes writable
{
    my role R is rw { has $.x; }
    my $o = R.new(x => 1);
    $o.x = 42;
    is $o.x, 42, "is rw on role makes attribute writable";
}

# Role without `is rw` keeps public attributes readonly
{
    my role R { has $.x; }
    my $o = R.new(x => 1);
    dies-ok { $o.x = 42 }, "role without is rw has readonly attribute";
    is $o.x, 1, "readonly attribute value unchanged";
}

# `is readonly` overrides `is rw` on the role
{
    my role R is rw { has $.x; has $.y is readonly; }
    my $o = R.new(x => 1, y => 2);
    $o.x = 10;
    is $o.x, 10, "is rw attribute can be changed";
    dies-ok { $o.y = 20 }, "is readonly overrides is rw on role";
    is $o.y, 2, "readonly attribute unchanged";
}

# Class consuming `is rw` role inherits rw attributes
{
    my role R is rw { has $.x; has $.y is readonly; }
    my class C does R { }
    my $o = C.new(x => 1, y => 2);
    $o.x = 10;
    is $o.x, 10, "class consuming is rw role has rw attribute";
    dies-ok { $o.y = 20 }, "is readonly survives role composition";
    is $o.y, 2, "readonly attribute unchanged in class";
}

# `also is rw` in role body
{
    my role R { has $.x; also is rw; has $.y is readonly; }
    my $o = R.new(x => 1, y => 2);
    $o.x = 10;
    is $o.x, 10, "also is rw makes attribute writable";
    dies-ok { $o.y = 20 }, "is readonly overrides also is rw";
}
