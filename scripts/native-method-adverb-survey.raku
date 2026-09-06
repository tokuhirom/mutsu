#!/usr/bin/env raku

# Regenerates the evidence behind `native_method_accepted_nameds`
# (`src/builtins/accepted_nameds.rs`): for each method name, the set of named
# parameters Rakudo declares for it, across every owner type that has one.
#
# Run with a real `raku` (see .agents/skills/install-raku/):
#
#     raku scripts/native-method-adverb-survey.raku
#     raku scripts/native-method-adverb-survey.raku chop polymod rotor
#
# The output is the authoritative accepted-named set for a method: a Raku method
# carries an implicit `*%_`, so a named argument that is not in this set cannot
# change the method's answer, and mutsu's builtin dispatch may therefore drop it
# before choosing an arity. `*%_` / `*%a` slurpies are reported as `**SLURPY**`
# and are NOT accepted names -- but note that a routine which validates its own
# `%_` (`grep`, `first`) accepts names that no signature mentions, so a method
# whose only entry is `**SLURPY**` still needs its adverbs confirmed by hand
# against `raku-doc/doc/Type/`.

my @OWNERS = <Mu Any Cool Str Int Num Rat Complex List Array Seq Hash Map Range
              Pair Set Bag Mix SetHash BagHash MixHash Blob Buf Match Date
              DateTime IO::Path Junction Supply Channel>;

my @DEFAULT-METHODS = <
    chop chomp polymod expmod base fmt sprintf AT-POS EXISTS-POS AT-KEY
    EXISTS-KEY subbuf subbuf-rw int-bounds join tail skip head combinations
    permutations indent samecase samemark unimatch uniprops roots minmax rotor
    classify categorize first grep map split comb lines batch Str contains
    starts-with ends-with substr-eq subst trans match min max unique squish
    sort reduce produce keys values kv pairs list Array
>;

sub named-params($m) {
    my %seen;
    for $m.candidates -> $c {
        for $c.signature.params -> $p {
            next unless $p.named;
            if $p.slurpy {
                %seen{'**SLURPY**'} = True;
            } else {
                %seen{$_} = True for $p.named_names;
            }
        }
    }
    %seen.keys.sort
}

my @methods = @*ARGS ?? @*ARGS !! @DEFAULT-METHODS;

for @methods -> $name {
    my %union;
    my @owners-with;
    for @OWNERS -> $owner {
        my $type = ::($owner);
        next if $type ~~ Failure;
        my $m = $type.^lookup($name);
        next without $m;
        my @n = named-params($m);
        next unless @n;
        @owners-with.push: $owner;
        %union{$_} = True for @n;
    }
    my @accepted = %union.keys.grep({ $_ ne '**SLURPY**' }).sort;
    my $slurpy = %union<**SLURPY**> ?? ' (+*%_)' !! '';
    printf "%-14s %s%s\n", $name, (@accepted ?? @accepted.join(' ') !! '--'), $slurpy;
}
