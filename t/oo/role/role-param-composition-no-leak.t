use Test;

# Composing a parametric role runs its body with the role's parameters bound;
# a value parameter must not overwrite a same-named variable of the routine
# the composition happens in (CSS::Units' `role CSS::Units[\dimension, \units]`
# turned CSS::Properties' `:$units` default into the role argument).

plan 3;

role UY[\units] { sub mk($v) { $v } }
role UZ[\units] { method u { units } }

sub a(:$units = 'pt', :$vw = do { UY['px'].new; $units }) { $vw }
is a(), 'pt', 'a parameter default reads its own sibling, not the role argument';

sub b(:$units = 'pt', :$vw = do { my $x = 5 but UY['px']; $units }) { $vw }
is b(), 'pt', '... also when the role is mixed in';

is (5 but UZ['px']).u, 'px', 'the role still sees its own parameter';
