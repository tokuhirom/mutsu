use Test;

plan 6;

# Assigning an aggregate to the topic itemizes it when the topic aliases a
# Scalar (an element, a `$` variable); it must not when it aliases a whole
# `@` container (`given @a { .=reverse }`).
{ my @a = 1; for @a { $_ = [1,2] }; is @a[0].raku, '$[1, 2]', 'for: array into element'; }
{ my @a = 1; for @a { $_ = %(a=>1) }; is @a[0].raku, '${:a(1)}', 'for: hash into element'; }
{ my $t = 1; given $t { $_ = [1] }; is $t.raku, '$[1]', 'given $scalar'; }
{ my @c = 1,2; given @c { .=reverse }; is @c.raku, '[2, 1]', 'given @a .= keeps container'; }
{ my @e = 1; for @e { $_ = 5 }; is @e.raku, '[5]', 'plain value unchanged'; }
{ my @f = 1; @f.map({ $_ = [3] }); is @f.raku, '[[3],]', 'map topic'; }
