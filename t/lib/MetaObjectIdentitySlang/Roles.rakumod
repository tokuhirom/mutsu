# Roles of the fixture slang of t/lang/parsing/slang-meta-object-identity.t
# (#17415): a `routine-declarator` whose action reads the declared method's
# `meta-object` twice, mixing a role into the first answer and writing an
# attribute of that role through the second.
use nqp;
use experimental :rakuast;
unit module MetaObjectIdentitySlang::Roles;

role Tagged is export { has $.tag is rw }

role Grammar is export {
    token routine-declarator:sym<tagged> {
        'tagged' <.end-keyword> <method-def=.key-origin('method-def', 'method')>
    }
}

role Actions is export {
    method routine-declarator:sym<tagged>(Mu $/) {
        my $method = nqp::atkey($/.hash, 'method-def').ast;
        $method.meta-object does Tagged;
        $method.meta-object.tag = 'same-object';
        self.DECLARATOR($/, $method);
    }
}
