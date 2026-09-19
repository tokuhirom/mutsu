use v6.d;
use Test;

plan 2;

role NamedAttribute {
    has $.base-name = self.name.substr(2);
    has $.builder = 'build-' ~ self.base-name;
}

class AttributeLike {
    has $.name;
}

my $value = AttributeLike.new(:name('$!answer')) does NamedAttribute;
is $value.base-name, 'answer',
    'a role attribute accessor is available to a later role default';
is $value.builder, 'build-answer',
    'later role defaults observe earlier role attributes';
