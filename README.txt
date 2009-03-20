
=====  Installation  =====

Edit hlean.hs and make sure the correct hashCommand and hashCommandParse 
are commented out, depending on what is available on the system:
'Alternative 1' is common for FreeBSD systems, while 'Alternative 2 or 3'
should be appropriate for Linux systems.

$ runhaskell Setup.lhs configure
$ runhaskell Setup.lhs build
$ runhaskell Setup.lhs install

=======  Usage  ========

$ hlean [dirs]

h       -> Print Usage Help
q       -> Exit
(space) -> Keep
(enter) -> Keep (this one and all remaining duplicates)
d       -> Delete
D       -> Delete (this one and all remaining duplicates)
s       -> Skip
S       -> Skip (this one and all remaining duplicates)
m       -> Move/Rename

