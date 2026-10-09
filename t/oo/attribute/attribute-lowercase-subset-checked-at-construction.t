use v6;
use Test;

# Found via SBOM::CycloneDX: only attributes whose type name starts with an
# upper-case letter were type-checked at construction, so a lower-case
# `subset bom-ref` never rejected anything.

plan 9;

subset big of Int where * > 5;
subset ne of Str where *.chars > 0;
class D { has big $.r }
class E { has ne $.r is rw }

throws-like { D.new(r => 1) }, X::TypeCheck::Assignment, 'lower-case subset rejects at new';
is D.new(r => 9).r, 9, 'a satisfying value is stored';
throws-like { E.new(r => "") }, X::TypeCheck::Assignment, 'empty string rejected';
is E.new(r => "a").r, 'a', 'non-empty accepted';
lives-ok { D.new }, 'unset attribute is not checked';
lives-ok { class N { has int $.n }; N.new(n => 3) }, 'native types stay exempt';

# `bless` checks the attribute type too.
my %h = r => 1;
throws-like { D.bless(|%h) }, X::TypeCheck::Assignment, 'bless: lower-case subset rejects';
throws-like { class F { has Int $.r }; F.bless(r => "x") }, X::TypeCheck::Assignment, 'bless: class type rejects';
is D.bless(r => 9).r, 9, 'bless: a satisfying value is stored';
