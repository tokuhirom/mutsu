unit class SeenListWalker;
use SeenUtil;
use SeenWalker;

my %seen{List} = ();

has @.items;

method content {
    %seen{$@!items} = self;
    my $r = ast-of($@!items.map({ $_ ~~ SeenWalker ?? .content !! $_ }).List);
    %seen{$@!items}:delete;
    $r;
}
