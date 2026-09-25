use Test;

plan 6;

class Plain {
    has %.values{Str} of Int;
}

class Coercive {
    has %.values{Str} of Int();
}

is Plain.new.values.of.^name, 'Int',
    'an object-hash attribute keeps its value type after postfix of';
is Plain.new.values.keyof.^name, 'Str',
    'an object-hash attribute keeps its key type after postfix of';
is Coercive.new.values.of.raku, 'Int(Any)',
    'a coercion value type is accepted after an object-hash key type';
is Coercive.new.values.keyof.^name, 'Str',
    'a coercion object-hash attribute keeps its key type';

class Checked {
    has %.values{Int} of Str;
}

my %good{Int};
%good{42} = 'ok';
is Checked.new(values => %good).values{42}, 'ok',
    'the parsed key and value constraints accept matching values';
my %bad{Int};
%bad{42} = 7;
dies-ok { Checked.new(values => %bad) },
    'the parsed value constraint is enforced';
