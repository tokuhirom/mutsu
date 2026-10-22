use Test;

# An `is rw` parameter relayed to a typed `is rw`/`is raw` callee, or to a method
# whose rw parameter is `$_`, still aliases the original container (found via
# PDF::COS::Tie, #12907).
plan 9;

sub typed-rw(Str $a is rw) { $a = "new" }
sub typed-raw(Str $a is raw) { $a = "new" }
sub raw-direct($a is raw) { $a = "new" }
sub via-rw($l is rw) { typed-rw($l) }
sub via-raw-typed($l is rw) { typed-raw($l) }
sub via-raw-untyped($l is rw) { raw-direct($l) }

my $s = "old";
via-rw($s);
is $s, "new", 'rw param -> typed rw sub';
$s = "old";
via-raw-typed($s);
is $s, "new", 'rw param -> typed raw sub';
$s = "old";
via-raw-untyped($s);
is $s, "new", 'rw param -> untyped raw sub';

my %h = a => "old";
via-rw(%h<a>);
is %h<a>, "new", 'hash element through rw param -> typed rw sub';
%h = a => "old";
via-raw-untyped(%h<a>);
is %h<a>, "new", 'hash element through rw param -> raw sub';

class C {
    method topic-rw(Str $_ is rw) { $_ = $_ ~ "!" }
    method via-raw($a is raw) { self.topic-rw($a) }
    method via-rw($a is rw) { self.topic-rw($a) }
}
%h = a => "old";
C.new.via-raw(%h<a>);
is %h<a>, "old!", 'hash element through raw method param -> $_ rw method';
%h = a => "old";
C.new.via-rw(%h<a>);
is %h<a>, "old!", 'hash element through rw method param -> $_ rw method';
my $t = "old";
C.new.via-raw($t);
is $t, "old!", 'scalar through raw method param -> $_ rw method';

sub top($l is rw) { C.new.via-raw($l) }
%h = a => "old";
top(%h<a>);
is %h<a>, "old!", 'rw sub param -> raw method -> $_ rw method';
