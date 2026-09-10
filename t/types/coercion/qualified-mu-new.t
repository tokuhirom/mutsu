use Test;

plan 2;

class NewFromMu {
    has $.x;
    has $.y;

    method new($a, $b) {
        self.Mu::new(:x($a), :y($b));
    }
}

my $value;
lives-ok { $value = NewFromMu.new('j', 'k') }, 'qualified Mu::new delegates to the base constructor';
is-deeply [$value.x, $value.y], ['j', 'k'], 'qualified Mu::new initializes attributes';
