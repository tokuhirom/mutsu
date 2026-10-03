# Keep unexported unit module classes out of importers

Importing a unit module no longer makes its unexported classes available by
their bare short names. The first load and later imports both copy only exported
type aliases into the importer's package. Qualified access and the declaring
module's own short-name lookup still work.
