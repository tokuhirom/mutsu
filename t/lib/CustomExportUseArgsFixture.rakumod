our $fixture-marker is export = 'marker';

sub EXPORT(*@args) {
    Map.new('&custom-export-args' => sub { @args.join(',') })
}
