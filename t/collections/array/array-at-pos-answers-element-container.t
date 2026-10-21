use Test;

# `Array.AT-POS` is `is raw`: it answers the element's own container, so a
# bound result is assignable and a user `AT-POS` that `callsame`s hands its
# own callers an assignable element, also when read without an assignment
# (#12907, PDF::COS::Tie::Array).
plan 6;

my @a = 1, 2, 3;
my $c := @a.AT-POS(1);
$c = 20;
is @a.join(','), '1,20,3', '.AT-POS(1) is the element container';

my @b = [Any, 'b'];
my $d := @b.AT-POS(0);
$d = 7;
is @b.join(','), '7,b', 'an undefined element is writable too';

sub fill($x is rw) { $x = 'filled' }

role Tie {
    method AT-POS($pos) is rw {
        my $val := callsame;
        fill($val) unless $val.defined;
        $val;
    }
}

class TA is Array {
    also does Tie;
}

my $t = TA.new;
$t.push(Any);
$t.push('b');
is $t.[0], 'filled', 'a user AT-POS fills the element it callsame-d';
is $t.join(','), 'filled,b', 'and the array keeps it';

class BA is Array {
    method ASSIGN-POS($pos, $val) {
        my $lval = $val;
        self.BIND-POS($pos, $lval)
    }
    method push($val) { self[+self] = $val }
}

my $b = BA.new;
$b.push('x');
$b.push('y');
my $e := $b.AT-POS(0);
$e = 'z';
is $b.join(','), 'z,y', 'BIND-POS binds the element to the caller variable';
sub set($x is rw) { $x = 'viaf' }
set($b.[1]);
is $b.join(','), 'z,viaf', 'so a rw parameter can write the element';
