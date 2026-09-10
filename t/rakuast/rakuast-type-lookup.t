use v6;
use experimental :rakuast;
use Test;

# RakuAST type-object introspection: `.^methods` (no adverb), `.^lookup`, and
# `.^can` now answer from the same model metadata, over the model MRO.
#
# RakuAST model classes are native type objects with no entry in mutsu's class
# registry, so each metaobject operation has to consult that metadata
# explicitly. `.^methods(:local)`, `.^can` and `.^method_table` did;
# `.^methods` (the default) and `.^lookup` did not, and answered `()` and
# `(Mu)` for a method the other three could see.
#
# `Type/Metamodel/MethodContainer.rakudoc` fixes the three adverb cases:
#   * default  -- the class and its parents, stopping at Cool/Any/Mu
#   * :local   -- only what the class declares itself
#   * :all     -- everything, including the Any/Mu tail
# and `.^lookup` returns the Method object found along the MRO, or `(Mu)`.
#
# This file is mutsu-only: the method *names* are mutsu's own model API
# (`local_method_names` documents them as such), not Rakudo's compiler-internal
# `IMPL-*` surface, so a raku run would legitimately report a different set.

plan 11;

# --- .^methods (default) sees the class's own model methods ------------------
my @m = RakuAST::IntLiteral.^methods.map(*.name).sort;
is @m, <new value>, '.^methods lists the model constructor and accessor';

# --- .^methods(:local) agrees for a leaf class ------------------------------
is RakuAST::IntLiteral.^methods(:local).map(*.name).sort, @m,
    '.^methods(:local) agrees with the default for a leaf model class';

# --- :all adds the Any/Mu tail on top ---------------------------------------
my @all = RakuAST::IntLiteral.^methods(:all).map(*.name);
ok @all.elems > @m.elems, '.^methods(:all) is a superset of the default';
ok @all.grep('value'), '.^methods(:all) still contains the model accessor';

# --- .^lookup finds the same methods ----------------------------------------
is RakuAST::IntLiteral.^lookup('value').name, 'value',
    '.^lookup finds a model accessor';
is RakuAST::IntLiteral.^lookup('value').^name, 'Method',
    '.^lookup returns a Method object';
is RakuAST::IntLiteral.^lookup('no-such-method').gist, '(Mu)',
    '.^lookup returns (Mu) for a missing method';

# --- .^can agrees ------------------------------------------------------------
ok RakuAST::IntLiteral.^can('value'), '.^can finds a model accessor';
nok RakuAST::IntLiteral.^can('no-such-method'), '.^can rejects a missing method';

# --- the three stay in lockstep on another class -----------------------------
ok RakuAST::Sub.^methods.map(*.name).grep('body'),
    '.^methods sees a Sub model accessor';
is RakuAST::Sub.^lookup('body').name, 'body',
    '.^lookup sees the same Sub model accessor';
