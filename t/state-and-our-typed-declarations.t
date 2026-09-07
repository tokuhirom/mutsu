use v6;
use MONKEY-SEE-NO-EVAL;
use Test;

# Two divergences left over from
# `news/2026-09/typed-declaration-hoist-missing-in-most-block-forms.md`, whose
# `hoist_typed_var_decls` pre-pass deliberately skipped `state` and `our`:
#
#  1. `state TYPE $x` must be in effect from BLOCK START, exactly like
#     `my TYPE $x` -- Raku's declaration visibility does not distinguish them.
#  2. `our TYPE $x` must not compile at all: a package variable is reachable by
#     its qualified name from anywhere, so there is nowhere to enforce a
#     lexical constraint.
#
# Measured against rakudo 2026.07.

plan 18;

# --- 1. a `state` declaration is in effect at block start ----------------
my $x = 0;
{
    my $err = "NO-DIE";
    try { EVAL '$x = "abc"'; CATCH { default { $err = .^name } } };
    is $err, 'X::TypeCheck::Assignment',
        'a `state Int $x` later in the block constrains $x from block start';
    state Int $x;
}

# The `my` spelling, unchanged.
my $y = 0;
{
    my $err = "NO-DIE";
    try { EVAL '$y = "abc"'; CATCH { default { $err = .^name } } };
    is $err, 'X::TypeCheck::Assignment', 'and the `my` spelling still does';
    my Int $y;
}

# --- the hoist must not disturb `state` persistence ---------------------
sub counter() { state Int $n = 0; $n++; $n }
is counter(), 1, 'a typed state counter starts at 1';
is counter(), 2, 'and persists across calls';
is counter(), 3, 'and keeps persisting';

sub acc() { state Int @a; @a.push(@a.elems); @a.elems }
is acc(), 1, 'a typed state Array persists too';
is acc(), 2, 'across a second call';

my @seen;
for 1..3 { state Int $c = 0; $c++; @seen.push($c) }
is @seen.join(','), '1,2,3', 'a typed state in a loop body persists per iteration';

sub bad() { state Int $v; $v = "x"; }
is (try bad()) // 'refused', 'refused', 'the state type constraint is still enforced';

sub untyped-state() { state $s = 0; $s++; $s }
is untyped-state(), 1, 'an untyped state is unaffected';
is untyped-state(), 2, 'and still persists';

# --- 2. `our TYPE $x` does not compile ---------------------------------
sub compiles($src) { (try EVAL($src)) // 'refused' }

is compiles('our Int $ourint; 1'), 'refused', '`our Int $x` is refused';
is compiles('our Int @ourarr; 1'), 'refused', '`our Int @a` is refused';
is compiles('our Int %ourhash; 1'), 'refused', '`our Int %h` is refused';
is compiles('our Int \ourbind = 5; 1'), 'refused', '`our Int \x` is refused';

# rakudo still PARSES `our Int $x` -- `Q[our Int $x].AST` builds a
# `RakuAST::VarDeclaration::Simple` carrying both `scope => "our"` and its
# `type` (pinned by `t/rakuast-vardecl-scoped.t`) -- and refuses only to
# compile it. So the refusal lives in the compiler, not the parser.
is Q[our Int $x].AST.gist.contains('scope       => "our"'), True,
    'the declaration still parses into an AST';

# What must KEEP compiling: the constraint belongs to the constant / the
# routine's return type there, not to an `our` container.
is compiles('our Int constant OURK = 3; OURK'), 3, '`our Int constant` still compiles';
is compiles('our Int sub ourf() { 7 }; ourf()'), 7, '`our Int sub` still compiles';
