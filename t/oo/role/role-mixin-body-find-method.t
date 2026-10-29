use Test;

# From WriteOnceHash 0.0.9: a role body captures the type's original method
# before the role's own is added, and the role's override delegates to it:
#   my &AT-KEY := ::?CLASS.^find_method('AT-KEY');
# For `my %h does R` the captured method is Hash's, so calling it must not
# re-enter the override.

plan 4;

role R {
    my &AT-KEY := ::?CLASS.^find_method('AT-KEY');
    method AT-KEY(::?CLASS:D: $key is raw) is raw {
        self.EXISTS-KEY($key) ?? AT-KEY(self, $key) !! 'none'
    }
}

my %h{Any} does R = 42 => 'a';
is %h{42}, 'a', 'the override reaches the native AT-KEY for a present key';
is %h{43}, 'none', 'and answers its own value for an absent one';

role S {
    my &AT-KEY := ::?CLASS.^find_method('AT-KEY');
    method viaamp($key) { &AT-KEY(self, $key) }
    method AT-KEY($key) { 'override' }
}
my %g does S;
%g.ASSIGN-KEY('k', 7);
is %g.viaamp('k'), 7, '&AT-KEY(self, $key) calls the captured native method';
is %g<k>, 'override', 'while a plain subscript still reaches the override';
