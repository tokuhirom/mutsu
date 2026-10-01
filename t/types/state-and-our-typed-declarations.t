use v6;
use MONKEY-SEE-NO-EVAL;
use Test;

# Two divergences left over from
# `news/2026-09/typed-declaration-hoist-missing-in-most-block-forms.md`, whose
# `hoist_typed_var_decls` pre-pass deliberately skipped `state` and `our`:
#
#  1. `state TYPE $x` must be in effect from BLOCK START, exactly like
#     `my TYPE $x` -- Raku's declaration visibility does not distinguish them.
#  2. `our TYPE $x` compiles, yields the type object, and is enforced. It was
#     refused here at first (measured against rakudo 2026.07); rakudo v2026.09
#     accepts every spelling below and enforces the constraint on the
#     container, including through the package-qualified name.
#
# Measured against rakudo v2026.09.

plan 39;

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

# --- 2. `our TYPE $x` compiles and is enforced -------------------------
sub compiles($src) { (try EVAL($src)) // 'refused' }

is compiles('our Int $ourint; 1'), 1, '`our Int $x` compiles';
is compiles('our Int @ourarr; 1'), 1, '`our Int @a` compiles';
is compiles('our Int %ourhash; 1'), 1, '`our Int %h` compiles';
is compiles('our Int ($oura, $ourb); 1'), 1, '`our Int ($a, $b)` compiles';
is compiles('class OurAttrC { our Int $.x }; 1'), 1, '`our Int $.x` compiles';
is compiles('our Int constant OURK = 3; OURK'), 3, '`our Int constant` compiles';
is compiles('our Int sub ourf() { 7 }; ourf()'), 7, '`our Int sub` compiles';

# rakudo parses `our Int $x` into a `RakuAST::VarDeclaration::Simple` carrying
# both `scope => "our"` and its `type` (pinned by `t/rakuast-vardecl-scoped.t`).
is Q[our Int $x].AST.gist.contains('scope       => "our"'), True,
    'the declaration parses into an AST';

# The declared type is the variable's default and is enforced on assignment.
is compiles('our Int $ourdef; $ourdef.raku'), 'Int', '`our Int $x` is the Int type object';
is compiles('our Int $ourinit = 5; $ourinit'), 5, '`our Int $x = 5` holds 5';
throws-like { EVAL('our Int $ourbad = 1; $ourbad = "a"; 1') }, X::TypeCheck::Assignment,
    'assigning a Str to `our Int $x` is refused';
throws-like { EVAL('our Int ($ourc, $ourd) = 1, "s"; 1') }, X::TypeCheck::Assignment,
    'a destructuring `our Int ($a, $b)` checks each element';
is compiles('our Int ($ourm, $ourn) = 1, 2; $ourm + $ourn'), 3,
    'and binds valid elements';
is compiles('our Int @ourtyped; @ourtyped.WHAT.raku'), 'Array[Int]', '`our Int @a` is an Array[Int]';
is compiles('our Int %ourtypedh; %ourtypedh.WHAT.raku'), 'Hash[Int]', '`our Int %h` is a Hash[Int]';
throws-like { EVAL('our Int %ourhv = a => 1; %ourhv<b> = "x"; 1') }, X::TypeCheck::Assignment,
    'a typed `our %h` checks its values';

# The constraint lives on the container, so it holds through the
# package-qualified name as well, not only through the lexical alias.
package TypedOurPkg {
    our Int $v = 1;
    our Str $s = "a";
    our Int @a = 1, 2;
    our Int %h = a => 1;
    our sub bump() { $v++ }
}
$TypedOurPkg::v = 7;
is $TypedOurPkg::v, 7, 'a valid write through the qualified name is stored';
throws-like { $TypedOurPkg::v = "a" }, X::TypeCheck::Assignment,
    'an invalid write through `$Pkg::x` is refused';
is $TypedOurPkg::v, 7, '... and leaves the value alone';
throws-like { $TypedOurPkg::s = 5 }, X::TypeCheck::Assignment,
    'the same for a typed Str package variable';
TypedOurPkg::bump();
is $TypedOurPkg::v, 8, 'a routine of the package updates the same variable';
is @TypedOurPkg::a.WHAT.raku, 'Array[Int]', 'the qualified array keeps its type';
is %TypedOurPkg::h.WHAT.raku, 'Hash[Int]', 'the qualified hash keeps its type';
throws-like { @TypedOurPkg::a.push("x") }, X::TypeCheck::Assignment,
    'pushing the wrong type through `@Pkg::a` is refused';

# Around them: the untyped, `has` and `my` spellings are unchanged.
is compiles('class PlainOurAttr { our $.x }; 1'), 1, '`our $.x` untyped still compiles';
is compiles('class HasAttrC { has Int $.x }; 1'), 1, '`has Int $.x` still compiles';
is compiles('class MyAttrC { my Int $.x }; 1'), 1, '`my Int $.x` still compiles';
is compiles('my Int ($mya, $myb); 1'), 1, '`my Int ($a, $b)` still compiles';
