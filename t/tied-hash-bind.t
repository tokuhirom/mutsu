use Test;

plan 9;

# A `my %h is CustomClass` (Associative) routes the `:=` element bind and the
# `:exists` / `:delete` adverbs through the class's BIND-KEY / EXISTS-KEY /
# DELETE-KEY methods rather than treating the variable as a plain Hash.

my @log;

class MyHash does Associative {
    has %.store;
    method AT-KEY($k)          is raw { %!store.AT-KEY($k) }
    method ASSIGN-KEY($k, \v)  is raw { %!store.ASSIGN-KEY($k, v) }
    method EXISTS-KEY($k)  { @log.push("EXISTS $k"); %!store.EXISTS-KEY($k) }
    method DELETE-KEY($k)  { @log.push("DELETE $k"); %!store.DELETE-KEY($k) }
    method BIND-KEY($k, \v) { @log.push("BIND $k"); %!store.BIND-KEY($k, v) }
    method keys()          { %!store.keys }
}

my %h is MyHash;
%h<a> = 1;
%h<b> = 2;

# --- :exists routes through EXISTS-KEY ----------------------------------------
is %h<a>:exists, True,  ':exists is True for a present key';
is %h<z>:exists, False, ':exists is False for a missing key';
ok @log.grep('EXISTS a'), ':exists dispatched to EXISTS-KEY';

# --- :delete routes through DELETE-KEY ----------------------------------------
is %h<b>:delete, 2, ':delete returns the removed value';
is %h.keys.sort.join(','), 'a', ':delete removed the key from the class';
ok @log.grep('DELETE b'), ':delete dispatched to DELETE-KEY';

# --- := element bind routes through BIND-KEY ----------------------------------
my $x = 99;
%h<c> := $x;
is %h<c>, 99, ':= bind stores the bound value (not the internal marker)';
ok @log.grep('BIND c'), ':= dispatched to BIND-KEY, not ASSIGN-KEY';

is %h.keys.sort.join(','), 'a,c', 'the tied hash reflects the bound key';
