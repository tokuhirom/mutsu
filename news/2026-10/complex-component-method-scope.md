# Restrict Complex component methods to Complex receivers

The `re` and `im` methods now reject non-`Complex` receivers, matching Rakudo's
method declarations and preventing numeric or string coercion from inventing
those methods.
