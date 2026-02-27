use Test;

plan 7;

my @a := array[int].new(42, 666);

is (@a.AT-POS(0) = 65), 65, ".AT-POS assignment in expression context returns assigned value";
ok @a.EXISTS-POS(0), ".EXISTS-POS returns true for existing index";
ok !@a.EXISTS-POS(3), ".EXISTS-POS returns false for missing index";
is @a.ASSIGN-POS(2, 33), 33, ".ASSIGN-POS returns assigned value";
is @a.AT-POS(2), 33, ".ASSIGN-POS updates array element";
throws-like { @a.BIND-POS(0, 1) }, Exception,
  message => "Cannot bind to a natively typed array",
  ".BIND-POS dies on natively typed arrays";
throws-like { @a.DELETE-POS(0) }, Exception,
  message => "Cannot delete from a natively typed array",
  ".DELETE-POS dies on natively typed arrays";
