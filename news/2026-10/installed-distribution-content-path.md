# Installed distribution content resolves provided modules

Installation metadata now retains each provided module's distribution-relative path. Installed distributions resolve that path to the copied source file, and module loading reads the resulting nested metadata shape.
