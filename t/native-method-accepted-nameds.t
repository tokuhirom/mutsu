use Test;

# A Raku *method* carries an implicit `*%_`, so a named argument the method does
# not declare is swallowed and cannot change the answer. mutsu's builtin methods
# are dispatched by arity, so an undeclared named used to occupy a positional
# slot and be numified or consumed as data. `src/builtins/accepted_nameds.rs`
# states which names each surveyed method accepts; this file pins both halves of
# that declaration:
#
#   * an UNDECLARED named is invisible -- `M(..., :zzz)` eqv `M(...)`;
#   * every DECLARED adverb still reaches the implementation.
#
# Every expectation below was measured against Rakudo (see the PR that added
# this file, and `scripts/native-method-adverb-survey.raku` for the accepted-name
# survey the Rust table is generated from). Rows that already agreed before the
# fix are pinned too, so a future change cannot quietly break them.

# --- Str / Cool ------------------------------------------------------------

is "abc".chop(:zzz), "ab", 'chop ignores an undeclared named';
is "abc".chop(2, :zzz), "a", 'chop(N) ignores an undeclared named';
is "abc\n".chomp(:zzz), "abc", 'chomp ignores an undeclared named';
is "abc".samecase("AB", :zzz), "ABC", 'samecase ignores an undeclared named';
is "abc".samemark("a\x[301]", :zzz), "a\x[301]b\x[301]c\x[301]",
    'samemark ignores an undeclared named';
is-deeply "abc".uniprops(:zzz).List, ("Ll", "Ll", "Ll"), 'uniprops ignores an undeclared named';
is "abcdef".indent(2, :zzz), "  abcdef", 'indent ignores an undeclared named';
is "%s".sprintf("x", :zzz), "x", 'sprintf ignores an undeclared named';

# --- numeric ---------------------------------------------------------------

is-deeply 10.polymod(3, :zzz).List, (1, 3), 'polymod ignores an undeclared named';
is-deeply 10.polymod(3, 2, :zzz).List, (1, 1, 1), 'polymod(a, b) ignores an undeclared named';
is 3.expmod(2, 5, :zzz), 4, 'expmod ignores an undeclared named';
is 255.base(16, :zzz), "FF", 'base ignores an undeclared named';
is 3.fmt("%d", :zzz), "3", 'Int.fmt(format) ignores an undeclared named';
is 255.fmt(:zzz), "255", 'Int.fmt ignores an undeclared named';
is (1,2,3).fmt(:zzz), "1 2 3", 'List.fmt ignores an undeclared named';
is (1,2,3).fmt("%02d", :zzz), "01 02 03", 'List.fmt(format) ignores an undeclared named';

# --- positional/associative protocol ---------------------------------------

is (1,2,3).AT-POS(1, :zzz), 2, 'AT-POS ignores an undeclared named';
is (1,2,3).EXISTS-POS(1, :zzz), True, 'EXISTS-POS ignores an undeclared named';
is {a => 1}.AT-KEY("a", :zzz), 1, 'AT-KEY ignores an undeclared named';
is {a => 1}.EXISTS-KEY("a", :zzz), True, 'EXISTS-KEY ignores an undeclared named';

# --- list shaping ----------------------------------------------------------

is (1,2,3).join("-", :zzz), "1-2-3", 'join ignores an undeclared named';
is-deeply (1,2,3).tail(2, :zzz).List, (2, 3), 'tail ignores an undeclared named';
is-deeply (1,2,3).skip(1, :zzz).List, (2, 3), 'skip ignores an undeclared named';
is (1,2,3).combinations(2, :zzz).elems, 3, 'combinations ignores an undeclared named';
is (1,2).permutations(:zzz).elems, 2, 'permutations ignores an undeclared named';
is-deeply (1,2,3).minmax(:zzz), (1..3), 'minmax ignores an undeclared named';
is (1..5).int-bounds(:zzz).join(","), "1,5", 'int-bounds ignores an undeclared named';

# `:by` is a DECLARED adverb of minmax and must still work.
is-deeply (1,2,3).minmax(:by(* * -1)), (3..1), 'minmax keeps its declared :by';

# --- Blob ------------------------------------------------------------------

is Buf.new(1,2,3).subbuf(1, :zzz).elems, 2, 'subbuf ignores an undeclared named';

# --- rotor: one declared adverb (:partial) ---------------------------------

is (1,2,3).rotor(2, :zzz).elems, 1, 'rotor ignores an undeclared named';
is (1,2,3).rotor(2, :partial).elems, 2, 'rotor keeps its declared :partial';
# A `Pair` in a POSITIONAL slot is a rotor cycle spec, not an adverb, and must
# survive (named-ness is a call-site property, ADR-0021).
is-deeply (1,2,3,4).rotor(2 => -1).List.map(*.List).List, ((1,2), (2,3), (3,4)),
    'rotor keeps a positional Pair cycle spec';

# --- classify / categorize: two declared adverbs (:as, :into) --------------

is (1,2,3).classify({$_}, :zzz).keys.elems, 3, 'classify ignores an undeclared named';
is (1,2,3).categorize({$_}, :zzz).keys.elems, 3, 'categorize ignores an undeclared named';
is-deeply (1,2,3).classify({$_}, :as({$_ * 2})).values.map(*[0]).sort.List, (2, 4, 6),
    'classify keeps its declared :as';
is-deeply (1,2,3).categorize({$_}, :as({$_ * 2})).values.map(*[0]).sort.List, (2, 4, 6),
    'categorize keeps its declared :as';
my %into;
(1,2,3).classify({$_}, :into(%into));
is %into.keys.elems, 3, 'classify keeps its declared :into';

# --- first / grep validate their adverbs instead of swallowing them --------

# Rakudo `fail`s an X::Adverb for an unknown adverb on `first` (and throws one
# on `grep`), rather than letting the implicit slurpy eat it.
{
    my $f = (1,2,3).first(:zzz);
    isa-ok $f, Failure, 'first(:unknown) is a Failure';
    is $f.exception.^name, 'X::Adverb', 'first(:unknown) fails with X::Adverb';
    is $f.exception.what, 'first', 'X::Adverb .what is the routine';
    is-deeply $f.exception.unexpected.List, ("zzz",), 'X::Adverb .unexpected lists the name';
    $f.so;  # mark handled; an unhandled Failure warns from DESTROY
}
{
    my $f = (1,2,3).first(* > 1, :zzz);
    isa-ok $f, Failure, 'first(matcher, :unknown) is a Failure too';
    $f.so;
}
{
    my $f = first(* > 1, (1,2,3), :zzz);
    isa-ok $f, Failure, 'the first SUB validates its adverbs as well';
    $f.so;
}
is (1,2,3).first(* > 1, :k), 1, 'first keeps its declared :k';
is (1,2,3).first(* > 1, :end), 3, 'first keeps its declared :end';
is-deeply (1,2,3).first(* > 1, :kv).List, (1, 2), 'first keeps its declared :kv';
is (1,2,3).first(* > 1), 2, 'first with no adverb is unaffected';
throws-like { (1,2,3).grep(* > 1, :zzz) }, X::Adverb, 'grep throws X::Adverb (unchanged)';
is-deeply (1,2,3).grep(* > 1, :k).List, (1, 2), 'grep keeps its declared :k';

# --- undeclared methods keep working exactly as before ---------------------
#
# These were already named-blind before the table existed; the table must not
# have changed them, and their own adverbs must still be honoured.

is-deeply "a,b,,c".split(",", :zzz).List, ("a", "b", "", "c"), 'split still ignores :zzz';
is-deeply "a,b,,c".split(",", :skip-empty).List, ("a", "b", "c"), 'split keeps :skip-empty';
is-deeply "abc".comb(:zzz).List, ("a", "b", "c"), 'comb still ignores :zzz';
is "a\nb".lines(:count), 2, 'lines keeps :count';
is-deeply "a\nb".lines(:zzz).List, ("a", "b"), 'lines still ignores :zzz';
is (1,2,3).batch(:elems(2)).elems, 2, 'batch keeps :elems';
is 42.Str(:superscript), "⁴²", 'Str keeps :superscript';
is 42.Str(:zzz), "42", 'Str still ignores :zzz';
is "abcdef".contains("B", :i), True, 'contains keeps :i';
is "abcdef".contains("b", :zzz), True, 'contains still ignores :zzz';
is "abcdef".starts-with("A", :i), True, 'starts-with keeps :i';
is "abcdef".substr-eq("B", 1, :i), True, 'substr-eq keeps :i';
is (1,2,3).min(:by(* <=> *)), 1, 'min keeps :by';
is "abcdef".subst("b", "X", :zzz), "aXcdef", 'subst still ignores :zzz';

# The implicit-`*%_` retry that fixed the LOUD half of this bug must keep
# working (news/2026-08/native-methods-honour-the-implicit-slurpy-named.md).
is 4.log(:base(2)), 4.log, 'log(:base) still falls back to the 0-ary log';
is "abc".uc(:foo), "ABC", 'uc(:foo) still swallows the named';
is (1,2,3).map({ $_ * 2 }).join("-", :foo), "2-4-6", 'a Seq body is consumed exactly once';

done-testing;
