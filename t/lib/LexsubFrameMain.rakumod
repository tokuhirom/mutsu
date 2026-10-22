use LexsubFrameOther;
class LexsubFrameMain {
    method listen($in) {
        supply {
            my $lock = Lock.new;
            whenever $in -> $x {
                emit helper($x);
            }
            sub helper($x) {
                my $r;
                $lock.protect: { $r = LexsubFrameOther::secret() + $x };
                $r
            }
        }
    }
}
