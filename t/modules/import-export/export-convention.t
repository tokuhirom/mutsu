use Test;

plan 15;

# Basic EXPORT::DEFAULT access for is export
sub exp_default is export { 'default_val' }
is exp_default(), 'default_val', 'direct call works';
is EXPORT::DEFAULT::exp_default(), 'default_val', 'EXPORT::DEFAULT::name() works';
is EXPORT::ALL::exp_default(), 'default_val', 'EXPORT::ALL::name() works';

# Custom tag export
sub exp_tagged is export(:my_tag) { 'tagged_val' }
is EXPORT::my_tag::exp_tagged(), 'tagged_val', 'EXPORT::custom_tag::name() works';
is EXPORT::ALL::exp_tagged(), 'tagged_val', 'EXPORT::ALL includes custom-tagged exports';

# Custom-tagged sub should NOT be in DEFAULT
ok ! &EXPORT::DEFAULT::exp_tagged, 'custom-tagged sub not in EXPORT::DEFAULT';

# Multiple tags
sub exp_multi_tag is export(:DEFAULT, :special) { 'multi_val' }
is EXPORT::DEFAULT::exp_multi_tag(), 'multi_val', 'multi-tag: accessible via DEFAULT';
is EXPORT::special::exp_multi_tag(), 'multi_val', 'multi-tag: accessible via custom tag';
is EXPORT::ALL::exp_multi_tag(), 'multi_val', 'multi-tag: accessible via ALL';

# is export with no parens (equivalent to :DEFAULT)
sub exp_bare is export { 'bare_val' }
is EXPORT::DEFAULT::exp_bare(), 'bare_val', 'bare is export goes to DEFAULT';
is EXPORT::ALL::exp_bare(), 'bare_val', 'bare is export accessible via ALL';

# Code reference access
ok &EXPORT::DEFAULT::exp_default.defined, '&EXPORT::DEFAULT::name is defined';
ok &EXPORT::ALL::exp_default.defined, '&EXPORT::ALL::name is defined';

# Package-qualified EXPORT access
{
    package Pkg {
        sub pkg_func is export { 'pkg_val' }
    }
    is Pkg::EXPORT::ALL::pkg_func(), 'pkg_val', 'Package::EXPORT::ALL::name() works';
    is Pkg::EXPORT::DEFAULT::pkg_func(), 'pkg_val', 'Package::EXPORT::DEFAULT::name() works';
}
