use Test;

plan 4;

# A method parameter's where constraint is evaluated while multi dispatch is
# building its deferral frame. The candidate matcher must retain the method's
# invocant for both the public $.attr spelling and explicit self.attr.
class PublicAttr {
    has $.limit = 10;
    multi method check($x where { $x > $.limit }) { 'over' }
    multi method check($x)                         { 'under' }
}
my $public = PublicAttr.new;
is $public.check(50), 'over', '$.attr is available in a method where constraint';
is $public.check(1),  'under', '$.attr can reject a candidate';

class ExplicitSelf {
    has $.limit = 10;
    multi method check($x where { $x > self.limit }) { 'over' }
    multi method check($x)                            { 'under' }
}
my $explicit = ExplicitSelf.new;
is $explicit.check(50), 'over', 'self.attr is available in a method where constraint';
is $explicit.check(1),  'under', 'self.attr can reject a candidate';
