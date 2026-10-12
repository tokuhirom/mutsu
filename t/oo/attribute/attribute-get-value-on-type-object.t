use Test;

# Attribute.get_value on a type object dies (Data::Dump relies on it:
# `try { Dump($attr.get_value($obj)) } // 'undefined'`).

plan 3;

role G { has $!g; }
class H does G { }
class P { has $.x = 5 }

my $attr = H.^attributes[0];
throws-like { $attr.get_value(H) }, Exception, :message(/'Cannot look up attributes in a H type object'/), 'type object';
is (try { $attr.get_value(H) } // 'undefined'), 'undefined', 'the try/// idiom';
is P.^attributes[0].get_value(P.new), 5, 'an instance still works';
