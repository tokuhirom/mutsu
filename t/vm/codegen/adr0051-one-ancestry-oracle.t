use Test;

# ADR-0051 P2/P5 (#9893): built-in type ancestry is read from one catalog,
# and `.^can` no longer claims methods raku resolves on neither the type nor
# its ancestors. Every expectation below is raku-verified.

plan 26;

# `.^can` false positives the deleted `Registry::builtin_mro_table` era left.
is Match.^can("succ").elems, 0, 'Match cannot succ';
is Match.^can("pred").elems, 0, 'Match cannot pred';
is Match.^can("base").elems, 0, 'Match cannot base';
is Match.^can("polymod").elems, 0, 'Match cannot polymod';
is Match.^can("parse-base").elems, 0, 'Match cannot parse-base';
is Any.^can("lazy").elems, 0, 'Any cannot lazy';
is Cool.^can("succ").elems, 0, 'succ is not a Cool method';
is Cool.^can("base").elems, 0, 'base is not a Cool method';
is Complex.^can("base").elems, 0, 'Complex cannot base';
is Pair.^can("lazy").elems, 0, 'Pair cannot lazy';

# ...while the genuine owners keep them.
is Str.^can("succ").elems, 1, 'Str can succ';
is Int.^can("base").elems, 1, 'Int can base';
is List.^can("lazy").elems, 1, 'List can lazy';
is Hash.^can("lazy").elems, 1, 'Hash can lazy (through Map)';
is Instant.^can("succ").elems, 1, 'Instant can succ (Real)';
is Duration.^can("polymod").elems, 1, 'Duration can polymod (Real)';

# `Instant`/`Duration` `succ`/`pred` step by one second.
is Instant.from-posix(1).succ.raku, Instant.from-posix(2).raku, 'Instant.succ';
is Instant.from-posix(2).pred.raku, Instant.from-posix(1).raku, 'Instant.pred';
is Duration.new(3).succ, 4, 'Duration.succ';
isa-ok Duration.new(3).pred, Duration, 'Duration.pred stays a Duration';

# Catalog roles answer type checks the old hardcoded MROs faked as parents.
ok Distribution::Path ~~ Distribution, 'Distribution::Path does Distribution';
ok Distribution::Hash ~~ Distribution, 'Distribution::Hash does Distribution';
ok CompUnit::Repository::FileSystem ~~ CompUnit::Repository::Locally,
    'CUR::FileSystem does CUR::Locally';
ok CompUnit::Repository::Installation ~~ CompUnit::Repository,
    'CUR::Installation does CUR';
is Distribution::Path.^mro.map(*.^name).join(" "), "Distribution::Path Any Mu",
    'Distribution::Path has no Distribution ancestor';

# A Cool-only name derived from the row catalog (P5) still gates a plain class.
dies-ok { class G { }; G.new.printf("x") }, 'plain class has no printf';
