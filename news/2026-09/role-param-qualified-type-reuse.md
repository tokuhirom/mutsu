`Event::Test` imported by a role body remains a usable type object when the
same qualified role parameter is checked again in the same process. Sigilless
arguments that are bare type objects are no longer mistaken for writable caller
variables and replaced by a container cell. (#8084)
