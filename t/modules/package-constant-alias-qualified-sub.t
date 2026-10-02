use Test;

plan 5;

module A::Meta {
    our sub index { 7 }
    our sub twice($n) { $n * 2 }
}

my constant Meta = A::Meta;

is Meta::index(), 7, 'Alias::sub() resolves through a constant naming a package';
is Meta::twice(4), 8, 'arguments pass through the aliased call';
is &Meta::index(), 7, '&Alias::sub() resolves through the constant';
is &Meta::index.(), 7, '&Alias::sub.() resolves through the constant';

class M { has &.index; method index { &!index() } }
class G { method module { my constant Meta = A::Meta; M.new(:index(&Meta::index)) } }
is G.module.index, 7, '&Alias::sub captured inside a method is callable';
