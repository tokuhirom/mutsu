use Test;

# A named `my method foo { }` / `my submethod foo { }` declaration is a
# `Method` / `Submethod` in raku, and `&foo` reports that. mutsu registered all
# of them as a plain `Sub`.
#
# Two independent losses, one on each side of the compiler:
#
#   * the parser's `my`/`our` declarator dispatch parsed `submethod` with the
#     shared `method_decl_body*` grammar and never set `is_submethod`, so the
#     two keywords were already indistinguishable by the time an AST existed;
#   * the `Stmt::MethodDecl` -> `Stmt::SubDecl` lowering dropped the declarator
#     again, and the `&name` value that `register_sub` builds for a method
#     declaration captured an env with no `__mutsu_callable_type` in it.
#
# The literal forms (`method (...) { }` and its `anon` spellings) were fixed
# separately and are pinned by `method-literal-is-a-routine.t`; this file is the
# named declarator statement.

plan 12;

# --- the type each named declarator reports -------------------------------

my method m1($x) { 1 }
is &m1.WHAT.^name, 'Method', '`my method` is a Method';

my submethod s1($x) { 1 }
is &s1.WHAT.^name, 'Submethod', '`my submethod` is a Submethod';

our method m2($x) { 1 }
is &m2.WHAT.^name, 'Method', '`our method` is a Method';

our submethod s2($x) { 1 }
is &s2.WHAT.^name, 'Submethod', '`our submethod` is a Submethod';

# `.^name` and `~~` agree with `.WHAT`.
is &m1.^name, 'Method', '.^name agrees for a method';
is &s1.^name, 'Submethod', '.^name agrees for a submethod';
ok &m1 ~~ Method, 'a named method smartmatches Method';
ok &s1 ~~ Submethod, 'a named submethod smartmatches Submethod';
ok &s1 ~~ Routine, 'a named submethod is a Routine';

# --- the neighbours that must not move ------------------------------------

my sub n1($x) { 1 }
is &n1.WHAT.^name, 'Sub', '`my sub` is still a Sub';

sub n2($x) { 1 }
is &n2.WHAT.^name, 'Sub', 'a plain `sub` is still a Sub';

our sub n3($x) { 1 }
is &n3.WHAT.^name, 'Sub', '`our sub` is still a Sub';

# --- deliberately NOT tested here: calling one ----------------------------
#
# The declarator rides on the captured environment of the very value `&name`
# resolves to, so it would be natural to check here that the routine still runs.
# It cannot be checked portably, because mutsu and rakudo disagree about the
# signature a named method value has -- rakudo's is `(Mu $:: $x, *%_)` and
# mutsu's is `($x)`, so every call that works in one is an arity error in the
# other, including `5.&m(21)`. That is a separate defect, tracked as
# tokuhirom/mutsu#8348; it survives this file's fix and is out of #8313's scope.
# Add the call / `.arity` / `.signature` assertions here once it is fixed.
