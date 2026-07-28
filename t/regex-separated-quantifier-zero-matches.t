use Test;

plan 8;

# `<item>* % sep` that matches nothing captures an EMPTY list under `$/<item>`,
# not one empty Match. YAMLish's `rule pairlist { <pair>* %% \, }` on "{}" must
# yield an empty Hash, not `{"" => Any}`.
grammar Sep {
    token item { 'a' }
    rule  trailing-sep { <item>* %% \, }
    rule  plain-sep    { <item>* %  \, }
    token token-sep    { <item>* %% \, }
    token no-sep       { <item>* }
}

for <trailing-sep plain-sep token-sep no-sep> -> $rule {
    my $m = Sep.subparse('', :rule($rule));
    ok $m, "$rule matches the empty string";
    is $m<item>.elems, 0, "$rule captures no items when it matched none";
}
