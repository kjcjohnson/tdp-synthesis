# Failures
The `mul-by-while` benchmark appears to be failing because:
 * tdp:derive is passed the outer spec, not the downward spec
 * tdp:derive tries to compute the value of the program set
 * ...but, the term being derived ($while [S]) uses a different state
 * and the outer spec is for [E], which has a different variable list.
Yuck.

The `plus-2-times-3` benchmark fails on pruning, because the N non-terminal
takes a different descriptor than the outer spec.
