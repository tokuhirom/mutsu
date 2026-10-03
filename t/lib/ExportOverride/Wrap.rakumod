# JSON::Fast::Hyper's shape: a `my proto` whose candidates call the imported
# `greet`, exported through `sub EXPORT` under that same name.
use ExportOverride::Base;
my proto sub greet-wrap(|) {*}
my multi sub greet-wrap(@_) { 'wrap[' ~ @_.map({ greet $_ }).join(',') ~ ']' }
BEGIN &greet-wrap.add_dispatchee(&greet);
our sub direct($x) { greet($x) }
sub EXPORT() {
    Map.new: ('&greet' => &greet-wrap, '&direct' => &direct)
}
