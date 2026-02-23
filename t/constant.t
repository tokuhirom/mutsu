use Test;

plan 12;

# Basic constant declarations
constant $a = 42;
is $a, 42, 'constant $a = 42';

constant $b = "hello";
is $b, "hello", 'constant $b = "hello"';

constant $c = 3.14;
is $c, 3.14, 'constant $c = 3.14';

# Constant with expression
constant $d = 10 + 20;
is $d, 30, 'constant with expression';

# Constant with $*PROGRAM
constant $prog = $*PROGRAM;
ok $prog.chars > 0, 'constant $prog = $*PROGRAM is not empty';
ok $prog ~~ IO::Path, 'constant $prog preserves IO::Path type';

# IO::Path.parent traversal
is ".".IO.parent.Str, "..", '.IO.parent from "."';
is ".".IO.parent(2).Str, "../..", '.IO.parent(2) from "."';
is "foo".IO.parent.Str, ".", 'parent of "foo"';
is "foo".IO.parent(2).Str, "..", 'parent(2) of "foo"';
is "a/b/c".IO.parent(3).Str, ".", 'parent(3) of "a/b/c"';
is "a/b/c".IO.parent(4).Str, "..", 'parent(4) of "a/b/c"';
