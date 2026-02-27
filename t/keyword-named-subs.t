use Test;

plan 3;

{
    sub sub($x) { $x }
    is sub("ok"), "ok", 'can call sub named "sub"';
}

{
    my ($x) = 5;
    sub my($value) { $value + 17 }
    is my($x), 22, 'can call sub named "my"';
}

{
    sub loop($x) { $x + 1 }
    is loop(5), 6, 'can call sub named "loop"';
}
