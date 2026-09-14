use v6;
use Test;

plan 4;

# A role's same-signature multi replaces the inherited candidate from its
# parent role. The parent must not become a second candidate when the child
# role is composed into a class.
role BaseRender {
    multi method render(Str $value) { "base:$value" }
}

role DerivedRender does BaseRender {
    multi method render(Str $value) { "derived:$value" }
}

lives-ok {
    EVAL 'class DerivedRenderConsumer does DerivedRender { }'
}, 'a child role override does not create a false composition conflict';
is EVAL('DerivedRenderConsumer.new.render("x")'), 'derived:x',
    'the child role multi wins over the inherited candidate';

# A parent role explicitly composed by the class remains an independent
# candidate, so the genuine conflict is still reported.
dies-ok {
    EVAL 'class ExplicitBaseConsumer does DerivedRender does BaseRender { }'
}, 'an explicitly composed ancestor still conflicts with the child override';

role OtherRender {
    multi method render(Str $value) { "other:$value" }
}

dies-ok {
    EVAL 'class UnrelatedRenderConsumer does DerivedRender does OtherRender { }'
}, 'unrelated same-signature role multis still require class resolution';
