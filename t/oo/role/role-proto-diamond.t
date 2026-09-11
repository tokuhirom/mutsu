use Test;

plan 14;

# The same role listed twice directly was already deduplicated and remains so.
my $direct = EVAL q:to/DIRECT/;
    role ProtoDiamond::DirectBase {
        proto rule directfiller {*}
        rule directfiller:sym<English> { 'en' }
    }
    grammar ProtoDiamond::DirectGrammar
        does ProtoDiamond::DirectBase
        does ProtoDiamond::DirectBase
    {
        rule TOP { <directfiller> }
    }
    ProtoDiamond::DirectGrammar.parse('en') ?? 'PARSED' !! 'NO MATCH'
    DIRECT
is $direct, 'PARSED', 'a role listed twice directly remains deduplicated';

# A proto regex declaration in a shared role must be installed once when the
# role is reached through a diamond.
role ProtoDiamond::RuleBase {
    proto rule filler {*}
}
role ProtoDiamond::RuleLeft does ProtoDiamond::RuleBase { }
role ProtoDiamond::RuleRight does ProtoDiamond::RuleBase { }
grammar ProtoDiamond::RuleGrammar does ProtoDiamond::RuleLeft does ProtoDiamond::RuleRight {
    rule TOP { 'x' }
}

ok ProtoDiamond::RuleGrammar.parse('x'),
    'a grammar can compose a shared proto rule through a diamond';

# Candidates from the shared role and one of its children must remain in the
# same composing grammar after the ancestor body is deduplicated.
role ProtoDiamond::CandidateBase {
    proto rule candidate {*}
    rule candidate:sym<English> { 'en' }
}
role ProtoDiamond::CandidateLeft does ProtoDiamond::CandidateBase {
    rule candidate:sym<Bulgarian> { 'bg' }
}
role ProtoDiamond::CandidateRight does ProtoDiamond::CandidateBase { }
grammar ProtoDiamond::CandidateGrammar
    does ProtoDiamond::CandidateLeft
    does ProtoDiamond::CandidateRight
{
    rule TOP { <candidate> }
}

ok ProtoDiamond::CandidateGrammar.parse('en'),
    'the shared role candidate still matches';
ok ProtoDiamond::CandidateGrammar.parse('bg'),
    'the child role candidate still matches';

# proto token and proto regex use the same deferred-body registration path.
role ProtoDiamond::TokenBase {
    proto token tokenfiller {*}
    token tokenfiller:sym<English> { 'en' }
}
role ProtoDiamond::TokenLeft does ProtoDiamond::TokenBase {
    token tokenfiller:sym<Bulgarian> { 'bg' }
}
role ProtoDiamond::TokenRight does ProtoDiamond::TokenBase { }
grammar ProtoDiamond::TokenGrammar
    does ProtoDiamond::TokenLeft
    does ProtoDiamond::TokenRight
{
    token TOP { <tokenfiller> }
}

ok ProtoDiamond::TokenGrammar.parse('en'),
    'proto token shared-role candidate still matches';
ok ProtoDiamond::TokenGrammar.parse('bg'),
    'proto token child-role candidate still matches';

role ProtoDiamond::RegexBase {
    proto regex regexfiller {*}
    regex regexfiller:sym<English> { 'en' }
}
role ProtoDiamond::RegexLeft does ProtoDiamond::RegexBase {
    regex regexfiller:sym<Bulgarian> { 'bg' }
}
role ProtoDiamond::RegexRight does ProtoDiamond::RegexBase { }
grammar ProtoDiamond::RegexGrammar
    does ProtoDiamond::RegexLeft
    does ProtoDiamond::RegexRight
{
    token TOP { <regexfiller> }
}

ok ProtoDiamond::RegexGrammar.parse('en'),
    'proto regex shared-role candidate still matches';
ok ProtoDiamond::RegexGrammar.parse('bg'),
    'proto regex child-role candidate still matches';

# A real duplicate in one body is not a repeated composition and must remain
# an X::Redeclaration.
throws-like q:to/ROLE-REDECL/, X::Role::Instantiation,
    role ProtoDiamond::BadRole {
        proto rule filler {*}
        proto rule filler {*}
    }
    class ProtoDiamond::BadRoleConsumer does ProtoDiamond::BadRole { }
    ROLE-REDECL
    'a genuine proto rule redeclaration is still rejected';

throws-like 'class ProtoDiamond::BadClass { proto rule filler {*}; proto rule filler {*} }',
    X::Redeclaration, 'a genuine class proto rule redeclaration is still rejected';

# Existing non-proto and method composition controls remain unchanged.
role ProtoDiamond::MethodBase { method hi { 'A' } }
role ProtoDiamond::MethodLeft does ProtoDiamond::MethodBase { }
role ProtoDiamond::MethodRight does ProtoDiamond::MethodBase { }
class ProtoDiamond::MethodClass
    does ProtoDiamond::MethodLeft
    does ProtoDiamond::MethodRight
{ }
is ProtoDiamond::MethodClass.new.hi, 'A',
    'a plain method still composes through a diamond';

role ProtoDiamond::PlainRuleBase { rule filler { 'x' } }
role ProtoDiamond::PlainRuleLeft does ProtoDiamond::PlainRuleBase { }
role ProtoDiamond::PlainRuleRight does ProtoDiamond::PlainRuleBase { }
grammar ProtoDiamond::PlainRuleGrammar
    does ProtoDiamond::PlainRuleLeft
    does ProtoDiamond::PlainRuleRight
{
    rule TOP { <filler> }
}
ok ProtoDiamond::PlainRuleGrammar.parse('x'),
    'a plain rule still composes through a diamond';

role ProtoDiamond::MethodProtoBase {
    proto method greet(|) {*}
    multi method greet(Int) { 'int' }
}
role ProtoDiamond::MethodProtoLeft does ProtoDiamond::MethodProtoBase {
    multi method greet(Str) { 'str' }
}
role ProtoDiamond::MethodProtoRight does ProtoDiamond::MethodProtoBase { }
class ProtoDiamond::MethodProtoClass
    does ProtoDiamond::MethodProtoLeft
    does ProtoDiamond::MethodProtoRight
{ }
my $method-proto = ProtoDiamond::MethodProtoClass.new;
is $method-proto.greet(1), 'int',
    'a proto method still dispatches its integer candidate';
is $method-proto.greet('x'), 'str',
    'a proto method still dispatches its string candidate';
