use Test;

# `use variables :D/:U` (#9990): the implicit smiley is part of the declared
# variable's constraint — enforced on every later assignment, not only at the
# declaration — and the pragma is lexical, so it never leaks out of its block
# or into routines declared elsewhere (e.g. `Test.rakumod`'s own `throws-like`).

plan 14;

{
    use variables :D;
    throws-like { my Int $x = 42; $x = Nil },
        X::TypeCheck::Assignment, symbol => '$x',
        ':D by pragma: reassigning Nil to an initialized scalar dies';
    throws-like { my Int $x = 42; $x = Int },
        X::TypeCheck::Assignment, symbol => '$x',
        ':D by pragma: reassigning a type object dies';
    throws-like { state Int $s = 1; $s = Int },
        X::TypeCheck::Assignment,
        ':D by pragma applies to state variables';
    throws-like { my Int @a; @a[0] = Int },
        X::TypeCheck::Assignment,
        ':D by pragma applies to array elements';
    throws-like { my Int %h; %h<k> = Int },
        X::TypeCheck::Assignment,
        ':D by pragma applies to hash values';
    throws-like { sub inner { my Int $y = 2; $y = Int }; inner() },
        X::TypeCheck::Assignment,
        'a routine declared inside the pragma scope inherits it';
    is { my Int:_ $x = 1; $x = Int; $x }(), Int,
        'an explicit :_ overrides the pragma';
    is { my Int $x is default(3) = 5; $x = Nil; $x }(), 3,
        'is default still supplies the reset value';
}

{
    use variables :U;
    throws-like { my Int $x; $x = 5 },
        X::TypeCheck::Assignment, symbol => '$x',
        ':U by pragma: reassigning a definite value dies';
    throws-like { my Int $a = 42 },
        X::TypeCheck::Assignment, symbol => '$a',
        ':U by pragma: throws-like from Test itself is unaffected';
}

{
    { use variables :D; }
    my Int $x;
    is $x, Int, 'the pragma does not leak out of its block';
    $x = Nil;
    is $x, Int, 'a later declaration outside the block is plain Int';
}

sub declared-outside() { my Int $y = 1; $y = Nil; $y }
{
    use variables :D;
    is declared-outside(), Int,
        'a routine declared outside the pragma scope is unaffected by a caller';
}

throws-like 'use variables :D; my Int $a', X::Syntax::Variable::MissingInitializer,
    implicit => ':D by pragma', 'a missing initializer still reports the implicit smiley';
