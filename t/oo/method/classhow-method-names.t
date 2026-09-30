use Test;

# From Statistics::Distributions (Utilities.rakumod, mixture-dist): it checks
# `'generate' ∈ $_.^method_names`.

plan 6;

class P { method pm { 1 } }
class A is P {
    has $.attr;
    method generate { 1 }
    submethod sm { 2 }
    method !priv { 3 }
}

ok A.^method_names.grep('generate'), 'declared method is listed';
ok A.^method_names.grep('sm'), 'submethod is listed';
ok A.^method_names.grep('attr'), 'accessor is listed';
nok A.^method_names.grep('pm'), 'inherited method is not listed';
ok 'generate' ∈ A.new.^method_names, 'works on an instance';
ok P.^method_names.grep('pm'), 'parent lists its own method';

