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

plan 19;

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

# --- the invocant is part of the signature (#8348) -------------------------
#
# A named method value carries an invocant, so it takes the receiver as its
# first argument. mutsu registered it with only the DECLARED parameters, while
# compiling the body against a leading `self` -- so the two disagreed by a slot:
# `.arity` was short by one, `&m($obj, ...)` and `$obj.&m(...)` were arity
# errors, and a body that mentioned `self` died with "Variable '$self' is not
# declared".

class Obj { }

my method call-me($x) { "{self.^name}/$x" }

is &call-me.arity, 2, 'a named method value counts its invocant';
is &call-me(Obj, 3), 'Obj/3', 'it takes the receiver as its first argument';
is Obj.&call-me(3), 'Obj/3', '...which is what `$obj.&name(...)` passes';
dies-ok { &call-me(3) }, 'and omitting the receiver is an arity error';

my submethod sub-call-me($x) { "{self.^name}/$x" }
is &sub-call-me(Obj, 4), 'Obj/4', 'a named submethod value binds its invocant too';

our method our-call-me($x) { "{self.^name}/$x" }
is &our-call-me(Obj, 5), 'Obj/5', 'and the `our` spelling';

# The body really does see the receiver, not just a positional named `self`.
my method reads-self() { self.^name }
is &reads-self(Obj), 'Obj', 'the body reads the receiver through `self`';
