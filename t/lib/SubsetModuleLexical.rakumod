my constant $header = "[\n\n";
my subset Hyperable of Str where .starts-with($header);
sub plain(Str() $text, :$immutable) { "plain" }
my proto sub fj(|) {*}
my multi sub fj(Hyperable:D $json) { "hyper" }
BEGIN &fj.add_dispatchee(&plain);
sub EXPORT() { Map.new(('&fj' => &fj,)) }
