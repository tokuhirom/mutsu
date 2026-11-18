# Loaded only through a `require` run inside a routine of
# RequireInRoutineLexA (t/modules/import-export/require-in-routine-file-scope-lexical.t).
my $lock = Lock.new;
my $cache = 42;
my %seen = a => 1;
sub plain()     { $cache }
sub publish()   { $lock.protect({ $cache }) }
sub bump()      { $lock.protect({ $cache++ }); $cache }
sub seen-keys() { my %next = %seen; %next<b> = 2; %next.keys.sort.join(',') }
class RequireInRoutineLexB {
    method plain     { plain() }
    method publish   { publish() }
    method bump      { bump() }
    method seen-keys { seen-keys() }
}
