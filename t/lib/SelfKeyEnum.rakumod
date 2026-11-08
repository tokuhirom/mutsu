# Fixture for t/modules/import-export/enum-key-named-self.t (#17197): an exported
# enum with a key spelled `self`, like Qwiratry's `WalkCursor` `Sector`.
unit module SelfKeyEnum;
enum Sector is export <self child parent>;
