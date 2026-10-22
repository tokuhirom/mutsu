unit class SeenWalker;
use SeenUtil;

my %seen{Hash} = ();

has %.data;

method content {
    %seen{$%!data} = self;
    my $r = ast-of($%!data);
    %seen{$%!data}:delete;
    $r;
}
