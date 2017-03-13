# CryptoVerif scripts for TextSecure/Signal

This directory contains CryptoVerif scripts for our variant of 
TextSecure/Signal.

TextSecure-AA.m4.cv, TextSecure-AB-BA.m4.cv, and TextSecure-KCI.m4.cv
are the master scripts from which runcv generates CryptoVerif scripts
using the m4 preprocessor.

* TextSecure-AA.m4.cv tests sessions in which Alice talks to herself.
* TextSecure-AB-BA.m4.cv tests sessions in which Alice talks to Bob
(and also includes sessions in which Bob talks to Alice).
* TextSecure-KCI.m4.cv tests for key compromise impersonation attacks.
(For this property, Alice talks to Bob and this file also includes sessions 
in which Bob talks to Alice.)

The generated files are:

* TextSecure-AA.$c.$r.cv
* TextSecure-AB-BA.$c.$r.cv
* TextSecure-KCI.$r.cv

where

* $c is either FS (for forward secrecy) or STD (standard, for the model
without compromise).
* $r is either NO_REPLAY_PROT (for the model of the initial 
protocol) or REPLAY_PROT (for the model with an additional replay
cache, to prevent replays when the one-time prekey is not used).

The file 'results' gives a quick summary of the results,
and 'runcv' is a script that generates the CryptoVerif models
as mentioned above and runs CryptoVerif on them.
(Please adapt the script to the location of CryptoVerif on
your computer if you run it.)
