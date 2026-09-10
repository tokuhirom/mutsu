use Test;

plan 4;

throws-like {
    Num.clone(:yes)
}, Exception, message => /'Cannot set attribute values when cloning a type object'/,
    'cloning a type object with attribute values dies';

is Num.clone.^name, 'Num', 'a type object still clones without arguments';

class CloneIssue7779 {
    has $.value;
}

my $original = CloneIssue7779.new(value => 2);
my $clone = $original.clone(value => 1);
is $clone.value, 1, 'an instance clone still applies the attribute override';
is $original.value, 2, 'an instance clone leaves the original unchanged';
