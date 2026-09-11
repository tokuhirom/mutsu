unit module EnumTaggedExportFixture;

sub tagged-control is export(:SubTag) { 'sub' }
our enum Plain is export(:PlainTag) <A B>;
enum Bare is export(:BareTag) <C D>;
our Str enum Typed is export(:TypedTag) «:E<e> :F<f>»;
