# The Object::Trampoline shape, for t/oo/mop/user-find-method-intercepts-calls.t.
use nqp;

class Lazy {
    has Mu $!code;
    has Mu $!result;
    has Lock $!lock;
    method ^find_method(Mu \type, Str:D $name) {
        my constant &proto-handler = proto method handler(|) {*}
        multi method handler(Lazy:U: |args) { "U $name" }
        multi method handler(Lazy:D \SELF: |args) is raw {
            if $name eq 'defined' || $name eq 'Bool' {
                False
            }
            else {
                $!lock.protect: {
                    if $!code {
                        my $object := $!code();
                        nqp::assign(SELF, $object) if nqp::iscont(SELF);
                        $!result := $object."$name"(|args);
                        $!code := Mu;
                    }
                    return-rw $!result
                }
            }
        }
        &proto-handler
    }
}
sub slack(&code) is export {
    nqp::p6bindattrinvres(
      nqp::p6bindattrinvres(nqp::create(Lazy), Lazy, '$!code', &code),
      Lazy,
      '$!lock',
      Lock.new
    )
}

