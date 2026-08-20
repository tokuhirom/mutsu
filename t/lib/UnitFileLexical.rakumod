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

# ADR-0039 slice 1: `@`/`%` file-scope lexicals get the same isolation as `$`
# above. Container mutation in mutsu is write-through-the-shared-node, so a
# module's own `push`/element-assign/whole-assign/key-set/`:delete` must
# reach ITS OWN container, never the loading script's same-named `my @items`
# / `my %items`.
my @items = <a b>;
my %items = (a => 1, b => 2);

sub arr-read() is export { @items.join(",") }
sub arr-push($v) is export { @items.push($v) }
sub arr-elem-assign($i, $v) is export { @items[$i] = $v }
sub arr-whole-assign() is export { @items = <p q> }

sub hash-read() is export {
    %items.sort(*.key).map({ "{.key}={.value}" }).join(",")
}
sub hash-key-set($k, $v) is export { %items{$k} = $v }
sub hash-delete($k) is export { %items{$k}:delete }
