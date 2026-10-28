module PkgNameBareword::Inner { our $v = 1; }

# A sub of this file naming the module it declares as a bare term.
our sub self-name { PkgNameBareword::Inner.^name }
our sub as-term { my \p = PkgNameBareword::Inner; p.^name }
