use Test;

# JSON::Class's unmarshaller binds a computed `do` result into a hash slot.
# The result is an aggregate and must stay one bound value rather than being
# itemized while the bind-index call is compiled.

plan 3;

my @items = <one two>;
my %args;
my $attr-name = 'str-array';
%args{$attr-name} := do if True { @items } else { Nil };

is %args<str-array>.raku, '["one", "two"]',
    'a computed aggregate binds as one hash value';
is %args<str-array>.elems, 2, 'the bound array retains its elements';

@items.push('three');
is %args<str-array>.elems, 3, 'the aggregate binding remains live';
