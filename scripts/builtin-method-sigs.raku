#!/usr/bin/env raku
# Regenerates src/runtime/builtin_method_sigs.tsv: the candidate signatures of
# every multi method of rakudo's core types, as `X::Multi::NoMatch` lists them
# ("Cannot resolve caller skip(Seq:D: Int:D, Int:D, Int:D); none of these
# signatures matches:" followed by one signature per line).
#
#   raku scripts/builtin-method-sigs.raku > src/runtime/builtin_method_sigs.tsv
#
# One row per candidate: `Type<TAB>method<TAB>signature`, in the order the
# dispatcher tries them. Every multi is listed, a one-candidate multi too (it
# still raises `X::Multi::NoMatch`). A method a type declares as an only method
# gets one `Type<TAB>method<TAB>-` row: it hides an ancestor's multi of the same
# name, and a bind failure on it is "Too many positionals passed", not NoMatch.
# A multi whose proto binds a bounded number of positionals (`proto method
# chars(*%)`) first gets a `Type<TAB>method<TAB>#<arity>-<count>` row (both
# counting the invocant): a call with more positionals than that fails the
# proto's own bind ("Too many positionals passed"), before any candidate.
my %seen;
my %groups;
for CORE::.keys.sort -> $name {
    my $t = CORE::{$name};
    next unless $t.HOW ~~ Metamodel::ClassHOW | Metamodel::ParametricRoleHOW
        | Metamodel::ParametricRoleGroupHOW | Metamodel::EnumHOW;
    my $tn = $t.^name;
    next if $tn.contains(any('<', '[', '::', '(')) || %seen{$tn}++;
    # A role's own method table holds its protos without their candidates;
    # the pun composes them (`Buf.allocate` lists Blob's and Buf's).
    # A role with stubbed methods cannot be punned (`Awaitable`): skip it.
    my $mt = $t;
    if $t.HOW ~~ Metamodel::ParametricRoleHOW | Metamodel::ParametricRoleGroupHOW {
        $mt = try $t.^pun;
        next if $!;
    }
    for $mt.^methods(:local) -> $m {
        unless $m.?is_dispatcher {
            %groups{$tn}{$m.name} = ['-'] unless %groups{$tn}{$m.name};
            next;
        }
        my @c = $m.candidates;
        # An only method and a multi of the same name: the multi answers.
        %groups{$tn}{$m.name} = [] if %groups{$tn}{$m.name} eqv ['-'];
        my $proto = $m.signature;
        %groups{$tn}{$m.name}.push("#{$proto.arity}-{$proto.count}")
            if $proto.count != Inf && !%groups{$tn}{$m.name};
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
