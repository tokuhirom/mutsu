# Exported classes keep their documentation

Leading and trailing Pod comments on a class declared `is export` now stay attached to the class instead of leaking onto its first method. This also covers exported unit classes. Fixes #12093.
