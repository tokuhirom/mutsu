# Fixture for t/modules/compunit/named-sub-expression-shadows-compunit-sub.t
# (JSON::Native shape): the file has its own `to-json`, and `EXPORT` builds a
# wrapper with a `sub to-json` EXPRESSION of the same name.
sub to-json($obj, *%opt) { "orig:" ~ %opt.sort.map({ .key ~ "=" ~ .value }).join(",") }
my %IMPL = 'to-json' => &to-json;
sub wrapped() is export {
    my %d = :pretty(False);
    my %subs;
    %subs<to-json> = sub to-json($obj, *%opt) { "wrapped:" ~ %d.sort.map({ .key ~ "=" ~ .value }).join(",") };
    %subs<to-json>
}
sub plain() is export {
    my $tag = 'plain';
    my $r = sub to-json($obj, *%opt) { "$tag:" ~ $obj };
    $r
}
sub EXPORT(*@args) {
    my %d = :pretty(False);
    my &orig = %IMPL<to-json>;
    Map.new("&to-json" => sub to-json($obj, *%opt) { orig($obj, |%d, |%opt) });
}
