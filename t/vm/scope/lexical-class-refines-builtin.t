use Test;

# `my class DateTime is DateTime { }` declares a lexical subclass that
# shadows a CORE type: the `is` parent is the outer CORE type, not the
# class being declared. DateTime::strftime builds its `:refine` export this
# way; mutsu rejected it with "'DateTime' cannot inherit from itself".

plan 10;

{
    my class DateTime is DateTime { }
    BEGIN DateTime.^add_method: "hello", method () { "hi " ~ self.year };

    my $dt := DateTime.new(2025, 1, 19, 14, 7, 42, :timezone(3600));
    is $dt.^name, 'DateTime', 'refined DateTime keeps its name';
    is $dt.hello, 'hi 2025', 'method added to the refinement is callable';
    ok $dt.WHAT === DateTime, '.new on the refinement builds the refinement';
    ok $dt ~~ CORE::DateTime, 'refinement is still a CORE DateTime';
    is DateTime.^mro.elems, 4, 'refinement MRO has the CORE type as parent';
}

ok DateTime.new(2025, 1, 1).^can('hello').not,
    'the refinement does not leak out of its block';

{
    my class Str is Str { method shout { self.uc ~ '!' } }
    is Str.new(value => 'ab').shout, 'AB!', 'Str can be refined too';
}

{
    my class Int is Int { method twice { self * 2 } }
    is Int.new(21).twice, 42, 'Int can be refined too';
}

throws-like 'my class Foobar is Foobar { }', X::Inheritance::SelfInherit,
    'a genuinely self-inheriting lexical class still dies';
throws-like 'class Barfoo is Barfoo { }', X::Inheritance::SelfInherit,
    'a genuinely self-inheriting package class still dies';
