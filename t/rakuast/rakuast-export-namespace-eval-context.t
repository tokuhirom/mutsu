use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

our $namespace-payload is export(:context) = 42;
package ExportNamespaceContextOwner {
    our $payload is export(:visible) = 17;
}

lives-ok { EVAL('EXPORT::context') }, 'a lowercase variable-export tag resolves in EVAL';
lives-ok { EVAL(q[EVAL('EXPORT::context')]) }, 'nested EVAL retains the caller tag namespace';
lives-ok { EVAL(q[ExportNamespaceContextOwner::EXPORT::visible].AST) },
    'an AST EVAL retains a qualified caller tag namespace';
lives-ok { EVAL('ExportNamespaceContextOwner::EXPORT::visible') },
    'a qualified variable-export tag resolves in EVAL';
ok EVAL('EXPORT::context') === EVAL('EXPORT::context'),
    'repeated EVAL keeps the same namespace';
throws-like { EVAL('EXPORT::missing-context') }, X::AdHoc,
    'an absent tag does not become a namespace';
throws-like { EVAL('ExportNamespaceContextOwner::EXPORT::missing-context') }, X::AdHoc,
    'an absent qualified tag stays missing';

done-testing;

