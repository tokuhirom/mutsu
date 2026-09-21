use Test;

# A runtime mixin of a parameterized role must bind its defaults for a
# qualified role-method call as well as for an unqualified call. JSON::Class
# uses this shape in Lumberjack::Message::JSON.

role QualifiedDefault[Bool :$enabled = False] {
    method value { $enabled.^name ~ ':' ~ $enabled.Bool }
}

class QualifiedDefaultConsumer { }
my $consumer = QualifiedDefaultConsumer.new;
$consumer does QualifiedDefault;

plan 2;
is $consumer.value, 'Bool:False',
    'unqualified runtime role calls bind default parameters';
is $consumer.QualifiedDefault::value, 'Bool:False',
    'qualified runtime role calls bind default parameters';
