curry-interface
===============

This package contains libraries to represent and read module interfaces of
Curry programs which are usually generated by the Curry front end and
stored in files with suffix `.icurry`.

The structure of these interfaces is defined in the module
`CurryInterface.Types`.
The module `CurryInterface.Files` contains operations to read `.icurry` files
and returns the structure of the interface.