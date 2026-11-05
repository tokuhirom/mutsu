#!/usr/bin/env raku
# Regenerates src/runtime/builtin_method_sigs.tsv: the candidate signatures of
# every multi method of rakudo's core types, as `X::Multi::NoMatch` lists them
# ("Cannot resolve caller skip(Seq:D: Int:D, Int:D, Int:D); none of these
# signatures matches:" followed by one signature per line).
#
#   raku scripts/builtin-method-sigs.raku > src/runtime/builtin_method_sigs.tsv
#
# One row per candidate: `Type<TAB>method<TAB>signature`, in the order the
# dispatcher tries them. Only methods with two or more candidates are listed
# (a single-candidate method reports its parameter mismatch instead).
my %seen;
my %groups;
for CORE::.keys.sort -> $name {
    my $t = CORE::{$name};
    next unless $t.HOW ~~ Metamodel::ClassHOW | Metamodel::ParametricRoleHOW | Metamodel::EnumHOW;
    my $tn = $t.^name;
    next if $tn.contains(any('<', '[', '::', '(')) || %seen{$tn}++;
    for $t.^methods(:local) -> $m {
        next unless $m.?candidates;
        my @c = $m.candidates;
        next if @c.elems < 2;
        my %once;
        for @c -> $cand {
            my $sig = $cand.signature.gist;
            %groups{$tn}{$m.name}.push($sig) unless %once{$sig}++;
        }
    }
}
# Sorted by type and method so a regeneration diffs cleanly; the candidates of
# one method keep the dispatcher's order.
for %groups.keys.sort -> $tn {
    for %groups{$tn}.keys.sort -> $mn {
        say "$tn\t$mn\t$_" for |%groups{$tn}{$mn};
    }
}
