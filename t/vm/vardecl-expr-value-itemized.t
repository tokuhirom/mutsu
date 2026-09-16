use Test;

# A scalar assignment itemizes what it STORES (`my $x = [1,2]; $x.raku` is
# `$[1, 2]`), but the VALUE THE ASSIGNMENT EXPRESSION ITSELF YIELDS used to
# skip that step: `(my $x = [1,2]).raku` rendered `[1, 2]`, dropping the `$`.
#
# Root cause: an inline `my $x = expr` used in expression position (`Grouped
# (DoStmt(VarDecl))`) compiles through `compile_expr_do_stmt`
# (src/compiler/expr_block.rs). Its plain-scalar arm computed the itemized
# value only for the STORE (`emit_set_named_var`, which runs the same
# `itemize_scalar_store` the statement-form `SetLocal`/`SetGlobal` paths use)
# but left the expression's own result as a `Dup` taken BEFORE that store --
# the un-itemized value -- whenever the declaration had no local slot of its
# own (the common case: a fresh, non-shadowing `my $x` at the top of a
# routine or file). Fixed by always reading the value back after the store
# (`emit_get_named_var`) instead of `Dup`-ing before it, so this expression's
# value is whatever the store itemized -- correct in both the local-slot and
# env-only cases alike.

plan 12;

is (my $a = [1, 2]).raku, '$[1, 2]', 'inline my with an Array initializer itemizes';
my @src = 1, 2;
is (my $b = @src).raku, '$[1, 2]', 'inline my with an Array-variable initializer itemizes';
my %hsrc = a => 1;
is (my $c = %hsrc).raku, '${:a(1)}', 'inline my with a Hash initializer itemizes';
is (my $d = (1, 2).Seq).raku, '$((1, 2).Seq)', 'inline my with a Seq initializer itemizes';
is (my $e = (1, 2)).raku, '$(1, 2)', 'inline my with a List initializer itemizes';
is (my $f = slip(5, 6)).raku, '$(slip(5, 6))', 'inline my with a Slip initializer itemizes';

# Reading the variable back on a following line was already correct -- this
# is purely about what the assignment expression itself yields.
my $g = [1, 2];
is $g.raku, '$[1, 2]', 'reading the variable back afterwards is unaffected (sanity)';

# Chained assignment: each `=` in the chain is itself an expression, so the
# itemization must survive being read as the RHS of an outer assignment too.
{
    my $h;
    my $i = $h = [3, 4];
    is $h.raku, '$[3, 4]', 'chained assignment: the inner target itemizes';
    is $i.raku, '$[3, 4]', 'chained assignment: the outer target itemizes too';
}

# An assignment used as a call argument: the itemization is observable as
# more than rendering (the callee gets an itemized `$`-container Array, not a
# flattening list).
sub describe($x) { $x.raku }
is describe(my $j = [5, 6]), '$[5, 6]',
    'an inline my used as a call argument itemizes';

# A declaration that already has a local slot of its own (shadowing an outer
# same-named lexical) went through the OTHER branch even before the fix;
# confirm it still itemizes too.
{
    my $k = 'outer';
    {
        is (my $k = [7, 8]).raku, '$[7, 8]',
            'a shadowing inline my (its own local slot) still itemizes';
    }
}

# A typed inline declaration keeps itemizing too (the fix's tail is shared by
# both the typed and untyped branches).
is (my Any $m = [9, 10]).raku, '$[9, 10]', 'a typed inline my still itemizes';

# vim: expandtab shiftwidth=4
