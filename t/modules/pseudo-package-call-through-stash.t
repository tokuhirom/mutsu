use Test;

plan 2;

throws-like {
    sub f { sub helper { 'h' }; MY::helper() }
    f()
}, X::Method::NotFound, message => /'CALL-ME'/,
    'a call through MY:: uses pseudo-stash lookup';

throws-like {
    sub f { sub helper { 'h' }; { OUTER::helper() } }
    f()
}, X::Method::NotFound, message => /'CALL-ME'/,
    'a call through OUTER:: uses pseudo-stash lookup';
