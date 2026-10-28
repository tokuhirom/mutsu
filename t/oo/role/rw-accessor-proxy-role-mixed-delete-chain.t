use Test;

plan 5;

my @log;
class Plain { has @.path; }
role Steps {
    method !child($key) is rw {
        my @next = |self.path, $key;
        my $node = Plain.new(:path(@next)) but Steps;
        Proxy.new(FETCH => -> $ { $node }, STORE => -> $, $value { @log.push: @next.join('/') ~ " = $value"; $value })
    }
    method AT-KEY($key) is rw { self!child($key) }
    method AT-POS($i)   is rw { self!child($i) }
    method DELETE-POS($i) { @log.push: (|self.path, $i).join('/') ~ " deleted"; 1 }
    method DELETE-KEY($k) { @log.push: (|self.path, $k).join('/') ~ " key-deleted"; 1 }
}
class Doc {
    method data is rw {
        my $root = Plain.new(:path[]) but Steps;
        Proxy.new(FETCH => -> $ { $root }, STORE => -> $, $ { })
    }
}
my $doc = Doc.new;
$doc.data<a><b>[0]:delete;
$doc.data<a><b>:delete;
$doc.data<a>:delete;
for ($doc,) { .data<p><q>[2]:delete }


is @log.join(' | '),
    'a/b/0 deleted | a/b key-deleted | a key-deleted | p/q/2 deleted',
    ':delete through an rw accessor reaches the role-mixed object at every depth';

@log = ();
my $n = $doc.data<a><b>;
$n[0]:delete;
is @log.join(' | '), 'a/b/0 deleted', 'a variable assigned the object deletes through it';

@log = ();
my $t := $doc.data<a><b>;
$t[1]:delete;
is @log.join(' | '), 'a/b/1 deleted', 'a variable bound to the rw Proxy deletes through what it fetches';

@log = ();
my $root = $doc.data;
$root<a><b>[2]:delete;
is @log.join(' | '), 'a/b/2 deleted', 'a nested :delete through a variable holding the root';

@log = ();
$root<m>:delete;
is @log.join(' | '), 'm key-deleted', 'a one-level :delete through the root';
