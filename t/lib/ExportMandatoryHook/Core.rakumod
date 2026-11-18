unit module ExportMandatoryHook::Core;
our sub infix:<hook-cat>($a, +@b) { "$a+{@b.join(',')}" }
