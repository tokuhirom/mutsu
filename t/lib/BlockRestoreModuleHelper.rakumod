unit module BlockRestoreModuleHelper;

our sub inner-part { 'inner' }

sub outer-call is export { inner-part() ~ '!' }
