use Test;

# `Array.AT-POS` is `is raw`: a user `AT-POS` that `callsame`s on an `is Array`
# subclass hands its own callers an assignable element, also when read without
# an assignment
# (#12907, PDF::COS::Tie::Array).
plan 3;

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
        self.BIND-POS($pos, $lval);
        $lval = $lval ~ '!';
    }
    method push($val) { self[+self] = $val }
}

my $b = BA.new;
$b.push('x');
$b.push('y');
is $b.join(','), 'x!,y!', 'BIND-POS binds the element to the caller variable, so a later write to it shows';
