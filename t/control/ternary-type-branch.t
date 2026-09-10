use v6;
use Test;

# Regression: a ternary whose then-branch is a bare type object / identifier
# immediately followed by `!!` was rejected at statement / `expression` level
# (and in a `constant ... is export = ...` RHS) because the parser assumed a
# bare identifier in then-position was the head of an unconsumed listop call.
# It must be accepted when `!!` follows directly.

plan 16;

# Bare statement-level ternary with a type-object then-branch.
is (1 ?? Int !! Str).^name, 'Int', 'ternary then-branch type object (Int) at expression level';
is (0 ?? Int !! Str).^name, 'Str', 'ternary else-branch type object selected';

# Native types as both branches.
is (1 ?? uint32 !! uint64).^name, 'uint32', 'native type then-branch (uint32)';
is (0 ?? uint32 !! uint64).^name, 'uint64', 'native type else-branch (uint64)';

# In a `constant ... is export = ...` RHS (the DBDish/NativeLibs shape).
constant cond = 4;
constant chosen is export = cond == 4 ?? uint32 !! uint64;
is chosen.^name, 'uint32', 'type-object ternary in `constant is export` RHS';

# Non-export constant with the same RHS (previously silently misparsed as a
# call to a routine named `constant`).
constant plain = 1 ?? Int !! Str;
is plain.^name, 'Int', 'type-object ternary in plain `constant` RHS';

# A literal then-branch still works (unchanged).
is (1 ?? 2 !! 3), 2, 'literal then-branch unaffected';

# Qualified type names (with ::) were already accepted; keep them working.
is (1 ?? Int !! Numeric).^name, 'Int', 'ternary with type-object branches, no false listop retry';

# A listop call in then-position (bareword head + comma args, NOT followed by
# `!!`) must still be parsed as a call consuming its args.
{
    my @log;
    sub note-it(*@a) { @log.push(@a.join(',')); 'T' }
    my $r = True ?? note-it(1, 2, 3) !! 'else';
    is @log[0], '1,2,3', 'listop then-branch still consumes its comma args';
}

# A bare (non-type) identifier in then-position is still rejected: it is a listop
# whose call gobbled the `!!` (raku: X::Syntax::ConditionalOperator::
# SecondPartGobbled). Only *type* barewords are accepted unparenthesized.
{
    sub rt123115 { 2 }
    dies-ok { EVAL q{1 ?? rt123115 !! 3} },
        'non-type bareword then-branch (listop gobbling !!) still errors';
}

# A *value* of a user-declared enum is a complete nullary term -- it takes no
# arguments, so it can never be the listop head the guard is looking for, and it
# must stand unparenthesized in then-position.
enum Light <RED AMBER GREEN>;
enum Status (OK => 200, MISSING => 404);

is (1 ?? RED !! AMBER).key, 'RED', 'angle-list enum value in then-position';
is (0 ?? RED !! AMBER).key, 'AMBER', 'and the else-branch still selects';
is (1 ?? OK !! MISSING).value, 200, 'pair-list enum value in then-position';

# The method-lvalue statement path parses its RHS in a mode where the guard does
# fire (unlike a `my $x = ...` RHS), which is where `DBDish::mysql` tripped:
# `.buffer_type = @!column-type[$col] ~~ Blob ?? MYSQL_TYPE_BLOB !! MYSQL_TYPE_STRING`.
class Holder { has $.slot is rw }
{
    my $h = Holder.new;
    given $h {
        .slot = 1 ?? RED !! AMBER;
    }
    is $h.slot.key, 'RED', 'enum value in a method-lvalue assignment RHS';
}

# An enum's values travel with it into an importing file.
{
    use lib $?FILE.IO.parent.add('lib-enum-ternary').Str;
    use EnumTernaryValues;
    is (1 ?? CRIMSON !! AZURE).key, 'CRIMSON', 'imported enum value in then-position';
    is (1 ?? FOUND !! GONE).value, 200, 'imported pair-list enum value in then-position';
}
