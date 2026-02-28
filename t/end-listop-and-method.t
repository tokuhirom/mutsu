use Test;

plan 8;

{
  my @a;
  is @a.end, -1, ".end works on uninitialized arrays";
}

{
  my $a;
  is $a.end, 0, ".end works on arbitrary scalars";
}

{
  my $a = [<a b c>];
  is $a.end, 2, ".end works on array items";
}

{
  throws-like 'end(1,2,3,4)', X::TypeCheck::Argument,
    "end(1,2,3,4) type-checks as one positional";
}

is (end (1,2,3,4)), 3, "end (1,2,3,4) works";
is (end [1,2,3,4]), 3, "end [1,2,3,4] works";
is (end ([1,2,3,4],)), 0, "end ([1,2,3,4],) returns 0";
is (end {a => 1, b => 2}), 1, "end works on hashes via elems-1";
