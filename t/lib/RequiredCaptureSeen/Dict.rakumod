# A Hash subclass whose own file-scope `%seen` and `@log` are written before
# it loads `RequiredCaptureSeen::Arr` at run time.
unit class RequiredCaptureSeen::Dict is Hash;
use RequiredCaptureSeen::Util :&note-key;
my %seen;
my @log;
method new {
    %seen<own> = 1;
    @log[0] = 'own';
    require ::('RequiredCaptureSeen::Arr');
    self.bless;
}
method note($k) { note-key $k }
method own { %seen.elems ~ '/' ~ @log.elems }
