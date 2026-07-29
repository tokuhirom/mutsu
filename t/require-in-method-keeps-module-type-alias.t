use v6;
use lib 't/lib';
use Test;

plan 11;

# A module body runs in the env of whatever frame triggered the load, so the
# short-name type aliases the module's own `use` statements install used to die
# with that frame. A `require` at file scope never noticed (the aliases outlived
# every later call, and even leaked to the script scope); a `require` inside a
# method frame lost them the moment the method returned, and the module's own
# methods could then no longer resolve their own imports.

class Installer {
    method install($name) {
        my \M = (require ::($name));
        M.new(:parent(Any))
    }
}
sub install-from-sub($name) {
    my \M = (require ::($name));
    M.new(:parent(Any))
}

{
    my $d = Installer.install('RequiredDriver');
    is $d.^name, 'RequiredDriver', 'require inside a method returns the class';
    is $d.widget-name, 'RequiredDriver::Native::Widget',
        'a method resolves a type its module imported, after the requiring frame is gone';
    is $d.widget-label, 'widget', 'and can instantiate it';
    is $d.widget-tag, 'tagged', 'an imported constant resolves the same way';
    is $d.slot-size, 42,
        'a method resolves its module\'s own file-scope constant';
    is $d.slot-size-via-sub, 43,
        'and so does a sub of that module called from the method';
    is $d.slot-name('big'), 'wide',
        'a method resolves its module\'s own file-scope `my` hash';
}

# A SECOND module importing the same names, loaded after the first. Nothing new
# reaches `env`, so the import itself has to attribute them to this module.
{
    my $d = Installer.install('RequiredDriver::Second');
    is $d.tag, 'tagged',
        'a second importer of an already-loaded module still owns the import';
    is $d.widget-name, 'RequiredDriver::Native::Widget',
        'and the type alias too';
}

{
    my $d = install-from-sub('RequiredDriver');
    is $d.widget-name, 'RequiredDriver::Native::Widget',
        'the same holds for a require inside a sub';
}

# The shape DBIish's `install-driver` actually uses: the `require` sits in a
# memoising `//= do { CATCH {...}; ... }` inside a `Lock.protect` callback, all
# inside a method, and the driver's methods run after all of that has unwound.
{
    my %installed;
    my $lock = Lock.new;
    class Registry {
        method install(%installed, $lock, $name) {
            $lock.protect: {
                %installed{$name} //= do {
                    CATCH { default { .rethrow } }
                    my \M = (require ::($name));
                    M.new(:parent(Any))
                }
            }
        }
    }
    my $d = Registry.install(%installed, $lock, 'RequiredDriver');
    is $d.widget-label, 'widget',
        'the memoised-inside-a-lock-inside-a-method shape resolves too';
}
