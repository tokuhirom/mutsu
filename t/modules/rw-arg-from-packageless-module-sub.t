use Test;

# A sub of a module file with no package declaration, called from the
# importer, passes its own variable to an `is rw` parameter. The callee is
# compiled on the fly there, and that path dropped the caller-variable names
# the binder needs, so the call died with "expects a writable container"
# (mutsu#10520; Color.new(:rgb) in CSS::TagSet).

plan 5;

use lib 't/lib';
use RwArgPackageless;

is rw-scalar(), 7, 'file-level sub writes a local through an rw param';
is rw-middle(), 7, 'rw param between two read-only ones';
is rw-for-topic().join(','), '7,7', 'loop topic passed to an rw param';
is rw-imported(), 255, 'imported rw sub called from a file-level sub';
is RwArgColor.new(rgb => [0, 300, 3]).g, 255, 'class method clips an Array() named param in place';
