use Test;

# From WriteOnceHash 0.0.9: its `!STORE-FROM-ITERABLE` calls `self.BIND-KEY`
# on a hash a role was mixed into.

plan 5;

role R {
    method fill(\pairs) {
        self.BIND-KEY(.key, .value) for pairs;
        self
    }
}

my %a does R;
%a.BIND-KEY("z", 2);
is %a<z>, 2, 'BIND-KEY on a role-mixed hash binds the key';
ok %a ~~ R, 'the role stays mixed in';

my %b{Any} does R;
%b.BIND-KEY(42, "a");
is %b{42}, "a", 'BIND-KEY with an Int key on a role-mixed object hash';

my %c does R;
%c.fill((x => 1, y => 2));
is-deeply %c<x y>, (1, 2), 'self.BIND-KEY inside a role method';

my $v = 5;
my %d does R;
%d.BIND-KEY("v", $v);
$v = 6;
is %d<v>, 6, 'the bound key aliases the caller variable';
