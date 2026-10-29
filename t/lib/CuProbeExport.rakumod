# EXPORT runs while the importing unit compiles; `$*CU` is that unit.
sub EXPORT {
    my $seen = $*CU.^name ~ '/' ~ $*CU.statement-list.^name;
    Map.new('&cu-seen' => sub { $seen })
}
