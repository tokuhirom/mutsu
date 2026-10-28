use Test;

plan 6;

my @log;

class Plain { has @.path; }

role Steps {
    method !child($key) is rw {
        my @next = |self.path, $key;
        my $node = Plain.new(:path(@next)) but Steps;
        Proxy.new(
            FETCH => -> $ { $node },
            STORE => -> $, $value { @log.push: @next.join('/') ~ " = $value"; $value },
        )
    }
    method AT-KEY($key) is rw { self!child($key) }
    method AT-POS($i)   is rw { self!child($i) }
}
class Doc {
    method data is rw {
        my $root = Plain.new(:path[]) but Steps;
        Proxy.new(FETCH => -> $ { $root }, STORE => -> $, $ { })
    }
}

my $doc = Doc.new;

$doc.data<a> = 1;
is @log.join(' | '), 'a = 1', 'a one-level store goes through the accessor Proxy';

@log = ();
$doc.data<a><b> = 2;
is @log.join(' | '), 'a/b = 2', 'a two-level store reaches the mixed-in AT-KEY';

@log = ();
$doc.data<a><b>[1] = 3;
is @log.join(' | '), 'a/b/1 = 3', 'a three-level store reaches the mixed-in AT-POS';

@log = ();
my $i = 4;
$doc.data<x><y>[$i] = 5;
is @log.join(' | '), 'x/y/4 = 5', 'a computed index reaches the store too';

@log = ();
for ($doc,) { .data<p><q> = 6 }
is @log.join(' | '), 'p/q = 6', 'a topic-rooted chain stores';

my $d2 = Doc.new;
$d2.data<m><n>[0] = 7;
is @log.join(' | '), 'p/q = 6 | m/n/0 = 7', 'another invocant stores through its own accessor';
