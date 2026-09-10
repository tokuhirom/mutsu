use Test;

plan 4;

# A CONTROL handler whose `when CX::Warn` arm resumes CONDITIONALLY (the
# `.resume` sits at the tail of an `if` inside the arm) must still resume the
# deep computation at the raise site — META6's t/030-versions.t counts
# version-prefix warnings this way and the assignment after the warn was lost.
sub make() { warn "W1"; 42 }
my $obj;
my $count = 0;
{
    CONTROL {
        when CX::Warn {
            if $_.message ~~ /W/ {
                $count++;
                $_.resume;
            }
        }
    };
    $obj = make();
}
is $obj, 42, 'the callee resumed and its return value survived';
is $count, 1, 'the handler ran exactly once';

# The unconditional form keeps working.
my $obj2;
my $count2 = 0;
{
    CONTROL {
        when CX::Warn {
            $count2++;
            $_.resume;
        }
    };
    $obj2 = make();
}
is $obj2, 42, 'unconditional resume arm still works';
is $count2, 1, 'and ran once';

done-testing;
