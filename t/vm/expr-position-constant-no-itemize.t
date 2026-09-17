use Test;

# A Raku `constant` has no Scalar container at all -- it binds its name
# directly to the raw value, like `:=` (`constant $x = [1,2,3]; $x.raku` is
# `[1, 2, 3]`, never `$[1, 2, 3]`). That holds for the anonymous
# `constant $ = EXPR` idiom too, and it must hold when the declaration is
# used INLINE, in expression position (`(constant $ = EXPR)`), not just at
# statement level.
#
# Root cause: `compile_expr_do_stmt` (src/compiler/expr_block.rs), which
# compiles a `Stmt::VarDecl` appearing in expression position, treated a
# `constant` exactly like a plain `my $x = EXPR` -- it never checked the
# declaration's `__constant` trait, so the anonymous form (which compiles to
# a `VarDecl` named `__ANON_STATE__` with `is_our: true`) got:
#  1. published via a plain `SetGlobal` instead of `SetGlobalRaw` (the
#     opcode the STATEMENT-position path already uses for a constant,
#     whose general-store handler skips itemization only under
#     `raw_mode`), and
#  2. unconditionally `WrapScalar`-boxed afterwards, a step meant only for
#     the *different*, genuinely-itemizing `my $ = EXPR` idiom that shares
#     the same synthetic `__ANON_STATE__` name.
# Passing a `(constant $ = [...])`/`(constant $ = blob32.new: ...)` value
# straight into a typed positional parameter, or into any other list-context
# consumer (`Z`, `|`), therefore saw ONE boxed item instead of the constant's
# real elements -- found via Digest::PSHA1's vendored SHA-1 dependency, whose
# `sha1-block` reduce accumulator is exactly such an anonymous constant, and
# produced a silently truncated (wrong) digest.

plan 8;

# Each `constant $ = ...` gets its own block: two anonymous `constant $`
# declarations in the SAME lexical scope collide ("Redeclaration of symbol
# '$'", matching rakudo), so every probe below is scoped independently.

{
    is (constant $ = [1, 2, 3]).raku, '[1, 2, 3]',
        'anonymous inline constant does not itemize its own .raku';
}

{
    is ((constant $ = [1, 2, 3, 4, 5]) Z+ [10, 20, 30, 40, 50]).raku,
        '(11, 22, 33, 44, 55).Seq',
        'anonymous inline constant flattens for Z (no Scalar-boxed single item)';
}

sub describe(@a) { @a.elems }
{
    is describe((constant $ = [1, 2, 3, 4, 5])), 5,
        'anonymous inline constant flattens into an @-sigil positional parameter';
}

sub zipadd(blob32 $a, blob32 $b) { ($a Z+ $b).raku }
{
    is zipadd((constant $ = blob32.new: 10, 20, 30, 40, 50), blob32.new(1, 2, 3, 4, 5)),
        '(11, 22, 33, 44, 55).Seq',
        'anonymous inline constant flattens into a typed blob32 positional parameter';
}

# The SHA-1 reduce shape itself: a named sub whose accumulator is an
# anonymous inline constant, combined via `reduce` with a slurpy of mapped
# chunks -- this is the exact pattern that silently truncated Digest::SHA's
# `sha1` under Digest::PSHA1.
sub combine(blob32 $acc, blob32 $chunk --> blob32) {
    blob32.new($acc Z+ $chunk);
}
my @chunks = blob32.new(1, 1, 1, 1, 1), blob32.new(2, 2, 2, 2, 2);
{
    is reduce(&combine, (constant $ = blob32.new: 0, 0, 0, 0, 0), |@chunks).raku,
        'Blob[uint32].new(3,3,3,3,3)',
        'reduce over an anonymous inline constant accumulator sees all elements';
}

# A NAMED constant declared inline must not itemize either.
is (constant FOO = [1, 2, 3]).raku, '[1, 2, 3]',
    'named inline constant does not itemize its own .raku';
is ((constant BAR = [1, 2, 3, 4, 5]) Z+ [10, 20, 30, 40, 50]).raku,
    '(11, 22, 33, 44, 55).Seq',
    'named inline constant flattens for Z too';

# Sanity: an ordinary (non-constant) inline `my $` still itemizes, unaffected
# by the fix above -- see t/vm/vardecl-expr-value-itemized.t for the full
# suite; this is just a guard that the two idioms did not collapse together.
is (my $x = [1, 2, 3]).raku, '$[1, 2, 3]',
    'sanity: an ordinary inline my $ still itemizes (unaffected by this fix)';

# vim: expandtab shiftwidth=4
