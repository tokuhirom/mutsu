unit module ContainerSlotLexical;

# A compunit's own file-scope containers, in every declarator ADR-0039 §4.1
# listed as an exclusion. Each pair of routines lets the consumer prove the
# module's binding is the one its own routines see, while the consumer's
# same-named container stays untouched.

my @items = <a b>;
my %hs = (k => 'v');
our @ours = <a b>;
our %ourh = (k => 'v');
state @st = <a b>;
my Int @ti = 1, 2;
my $anon = [<a b>];

sub my-push($v) is export { @items.push($v) }
sub my-peek() is export { @items.join(",") }
sub my-hset($k) is export { %hs{$k} = 1 }
sub my-hpeek() is export { %hs.keys.sort.join(",") }

sub our-push($v) is export { @ours.push($v) }
sub our-peek() is export { @ours.join(",") }
sub our-hset($k) is export { %ourh{$k} = 1 }
sub our-hpeek() is export { %ourh.keys.sort.join(",") }

sub state-push($v) is export { @st.push($v) }
sub state-peek() is export { @st.join(",") }

sub typed-push(Int $v) is export { @ti.push($v) }
sub typed-peek() is export { @ti.join(",") }

sub anon-push($v) is export { $anon.push($v) }
sub anon-peek() is export { $anon.join(",") }

sub dyn-push($v) is export { @*csldyn.push($v) }
sub dyn-peek() is export { @*csldyn.join(",") }
