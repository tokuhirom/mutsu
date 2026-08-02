unit module UnitFileLexical;

# File-scope lexicals of a compunit. A module body runs in the env of whatever
# frame loaded it, so without an isolating store these would be the very same
# storage as a same-named `my` in the loading script.
my $secret = "module";

sub peek() is export { $secret }
sub poke($v) is export { $secret = $v }

# A lazily-initialized lexical: the module writes it from a sub, long after the
# loading frame declared its own same-named variable (this is Test.rakumod's
# `_init_io` shape).
my $lazy;
sub lazy-init() is export { $lazy = "inited" unless $lazy; $lazy }
