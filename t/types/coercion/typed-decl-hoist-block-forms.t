use Test;

# Raku's "declarations are in effect at block start" rule: a `my TYPE $x`
# constrains the name for the WHOLE block, so an assignment that reaches the
# name before the textual declaration (here through EVAL) still type-checks
# against the declared type.
#
# The constraint is registered at block entry by the compiler's
# `hoist_typed_var_decls`. Every block-compilation entry point must do it: the
# statement-position `{ ... }` (BlockScope), routine and closure bodies, and
# loop / if branch bodies all used to skip the hoist, so the very same block
# lost its declared type just because it was not compiled through the
# value-position inline path.

plan 12;

my $x = 0;
my @a; #OK
my %h; #OK

# 1. statement-position bare block (a trailing statement forces `BlockScope`)
{
    dies-ok { EVAL '$x = "abc"' }, 'statement-position bare block';
    my Int $x; #OK
}

# 2. value-position bare block (the inline path, already covered before)
{
    {
        dies-ok { EVAL '$x = "abc"' }, 'nested value-position bare block';
        my Int $x; #OK
    }
}

# 3. block carrying a `use` statement (PushImportScope path)
{
    use nqp;
    dies-ok { EVAL '$x = "abc"' }, 'block with a use statement';
    my Int $x; #OK
}

# 4. routine body
sub in-sub() {
    dies-ok { EVAL '$x = "abc"' }, 'sub body';
    my Int $x; #OK
}
in-sub();

# 5. method body
class C {
    method m() {
        dies-ok { EVAL '$x = "abc"' }, 'method body';
        my Int $x; #OK
    }
}
C.new.m;

# 6. `for` loop body
for 1 {
    dies-ok { EVAL '$x = "abc"' }, 'for loop body';
    my Int $x; #OK
}

# 7. `while` loop body
my $n = 0;
while $n++ < 1 {
    dies-ok { EVAL '$x = "abc"' }, 'while loop body';
    my Int $x; #OK
}

# 8. C-style `loop` body
loop (my $i = 0; $i < 1; $i++) {
    dies-ok { EVAL '$x = "abc"' }, 'C-style loop body';
    my Int $x; #OK
}

# 9. `if` branch
if True {
    dies-ok { EVAL '$x = "abc"' }, 'if branch';
    my Int $x; #OK
}

# 10/11/12. other declared types reach the same registration
{
    dies-ok { EVAL '$x = 42' }, 'Str constraint';
    my Str $x; #OK
}
{
    dies-ok { EVAL '@a.push("abc")' }, 'typed array element type';
    my Int @a; #OK
}
{
    dies-ok { EVAL '%h<k> = "abc"' }, 'typed hash value type';
    my Int %h; #OK
}
