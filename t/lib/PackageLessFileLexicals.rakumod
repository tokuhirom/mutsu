# A module file with no `unit` declarator: its routines are declared in
# GLOBAL, and its file-scope `my`s are the compunit's own lexicals.
my $count = 0;
my @seen;
my %by-name;
my $label = 'module';

sub pfl-bump() is export { $count++; @seen.push($count); %by-name{"n$count"} = $count; $count }
sub pfl-count() is export { $count }
sub pfl-seen() is export { @seen.join(',') }
sub pfl-keys() is export { %by-name.keys.sort.join(',') }
sub pfl-label() is export { $label }
sub pfl-relabel($new) is export { $label := $new; $label }
sub pfl-in-block() is export { if True { $count } }
sub pfl-call(&code) is export { code() }
