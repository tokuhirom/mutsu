use RoleLexicalScopeTypes;
unit role RoleLexicalScopeRole;

has Flavour $!flavour = Sweet;

# Every name here is a bare term imported into THIS compunit. A composed
# role method used to run anchored on the consuming class, so none of them
# resolved and each degraded to a plain string.
method role-constant()     { MAGIC }
method role-sub()          { helper() }
method role-type()         { Handle.^name }
method role-enum-bare()    { Sour }
method set-flavour-bare()  { $!flavour = Sour; $!flavour }
method flavour()           { $!flavour }
