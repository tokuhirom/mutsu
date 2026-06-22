use Test;

# env<->locals coherence (env_dirty substrate, docs/captured-outer-cell-sharing.md
# §10): a custom HOW (built via Metamodel::Primitives.create_type) defines
# `type_check`/`accepts_type`/`find_method` methods that mutate a captured-outer
# caller scalar (typically a `++$counter`). Those methods run via the slow
# `run_instance_method_resolved` path during `~~` smartmatch type-checking, which
# merges the captured write into env but does NOT set `env_dirty` or record a
# precise writeback. The owning caller *local slot* was refreshed only by the
# blanket pull, and because `++$counter` is a read-modify-write, a stale slot
# loses the increment across successive smartmatch statements. The smartmatch op
# now drains a precise `pending_caller_var_writeback` recorded around the HOW
# dispatch. Each subtest must hold with the blanket reconcile ON (default) AND OFF
# (`MUTSU_NO_BLANKET_RECONCILE=1 MUTSU_NO_PRECISE_RECONCILE=1`).

plan 6;

{
    my $type-checks = 0;
    my $find-calls = 0;

    class UnionTypeHOW {
        has @!types;
        submethod BUILD(:@!types) { }
        method new_type(*@types) {
            my $how = self.new(:@types);
            Metamodel::Primitives.create_type($how, 'Uninstantiable');
        }
        method type_check(Mu $, Mu \check) {
            ++$type-checks;
            for |@!types, Any, Mu {
                return True if check<> =:= $_<>;
            }
            False;
        }
        method accepts_type(Mu $, Mu \check) {
            for @!types {
                return True if Metamodel::Primitives.is_type(check, $_);
            }
            False;
        }
        method find_method(Mu $, $name) {
            $find-calls++;
            Any.^find_method($name);
        }
    }

    my $int-or-rat = UnionTypeHOW.new_type(Int, Rat);
    $type-checks = 0;
    $find-calls = 0;

    # Before compose: type_check (LHS custom) and find_method (RHS custom) run.
    nok Int ~~ $int-or-rat, 'union broken before compose (RHS, find_method)';
    nok 420 ~~ $int-or-rat, 'union broken before compose (RHS value)';
    ok  $int-or-rat ~~ Int, 'union on LHS calls type_check';

    # The captured counters must reflect every increment, not a stale read.
    ok $type-checks > 0, 'type_check counter reaches caller slot';
    ok $find-calls >= 2, 'find_method counter accumulates across statements (not stuck at 1)';
}

# Second independent custom type to confirm the writeback is not a one-shot.
{
    my $checks = 0;
    class OneHOW {
        has $!t;
        submethod BUILD(:$!t) { }
        method new_type($t) {
            my $how = self.new(:$t);
            Metamodel::Primitives.create_type($how, 'Uninstantiable');
        }
        method type_check(Mu $, Mu \check) { ++$checks; check<> =:= $!t<> }
        method find_method(Mu $, $name) { Any.^find_method($name) }
    }
    my $only-int = OneHOW.new_type(Int);
    $checks = 0;
    $only-int ~~ Int;
    $only-int ~~ Str;
    is $checks, 2, 'two LHS-custom smartmatches increment the counter twice';
}
