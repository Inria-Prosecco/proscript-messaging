# Automated Verification for Secure Messaging Protocols and their Implementations: A Symbolic and Computational Approach
## Supporting Materials

### WARNING: EXPERIMENTAL SOFTWARE
PS2PV, the ProScript-to-ProVerif compiler, is currently in a highly experimental implementation state. Bugs are numerous and to be expected.

### Overview
This repository contains supporting materials for our paper, "Automated Verification for Secure Messaging Protocols and their Implementations: A Symbolic and Computational Approach":

- `cv` contains CryptoVerif models discussed in the paper.
- The remaining directories can be used to generate actual ProVerif models from actual ProScript code, that can then be evaluated for queries.
- `paper-longversion.pdf` is the long version of our paper with some additional details.

The messaging protocol implementation included in this repository is exactly the same code that is used by the [Cryptocat](https://crypto.cat) secure messaging software in production.

### Instructions

```
cd ps2pv
make
cd ..
make property-messages-principals-direction
```

This will generate a model by translating whatever is in `ps/sp.js` and adding the appropriate top-level process for the queries.

* Valid values for `property`: `secrecy`, `authenticity`, `forwardsecrecy`, `futuresecrecy`, `indistinguishability`
* Valid values for `messages`: `1`, `3` (or `2` which indicates a "single-flight scenario" for forward secrecy (see paper)).
* Valid values for `principals`: `AB` (Alice and Bob), `ABM` (Alice, Bob and compromised Mallory)
* Valid values for `direction`: `oneway`, `twoway`

**Note**: Not all combinations are current available. `ls tl` for current ones.

### Authors
Nadim Kobeissi, Karthikeyan Bhargavan and Bruno Blanchet.
Special thanks and acknowledgments to Antoine Delignat-Lavaud.
