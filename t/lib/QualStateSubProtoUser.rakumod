unit module QualStateSubProtoUser;
use QualStateSubProtoLib;

# `"{$0}"` declares an anonymous state var, which makes the module capture a
# shared body for this sub (`imported_compiled_fns`); that body is what a
# qualified call runs.
our sub gen($data) is export {
    my $r = rs($data);
    my $c = "a";
    my $tail = $c ~~ / (a) / ?? "a{$0}" !! "";
    "$r:$tail";
}
