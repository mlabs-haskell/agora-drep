# Agora DRep Governance Effects

## Introduction

This repository contains the source code for a new set of Plutus scripts that
allow the [the Agora governance system](https://github.com/Liqwid-Labs/agora)
to participate in Cardano governance proposals (see
[Cardano Governance](https://cardano.org/governance/)).

The general use case these scripts enable is the creation and handling of fully
automated, on-chain DReps that can vote on _pre-existing_ Cardano Governance
proposals (the creation of proposals is not supported).

The policy that decides when or how the DRep votes is not given, which allows
an Agora-based system to implement any suitable policy they desire. There already is
an implementation of an automatic DRep for Agora DAOs created with
[Clarity](https://www.clarity.vote/). See the [references](#references) at the end
of the page for more information on this.

In this document, an overview of the different scripts is given as well as instructions
on how to deploy them. At the end, two use cases are explained and references are provided
for more documents and websites of interest.

## Keywords

The following terms are used in the document:

* _Agora system_: any governance system implemented using the set of governance
  modules provided by [Agora](https://github.com/Liqwid-Labs/agora/tree/staging).

* _Agora DAO_: A specific Agora system implemented by [Clarity](www.clarity.vote).

* _DRep_: An actor in Cardano's governance system which can author and vote on
  Cardano governance proposals.

* _Authority token / GAT_: A utility token created automatically by an Agora
  system. These serve as "keys" that allow the execution of interesting on-chain
  effects and, in secure Agora systems, **represent the explicit approval of the
  Agora system's members**.

* _Effect / Effect script_: A script that is executed **only** when a an authority
  token is burned and which performs the desired actions of the Agora system's users.

## Overview

### The scripts

This repository contains the source code for three Plutus scripts:

* **The voting script**, which is [an Agora effect](https://liqwid.notion.site/Effects-3364731305544dc1972221b6d1353a69).

  This script is the one responsible for validating that the DRep votes as
  intended by the Agora system.

* **The proxy script**, that behaves as a compatibility wrapper.

  The need for this script is explained in detail in the technical specification of
  this repository.

* **The PlutusV3 authority token script**, which is needed for the
  "GAT upgrade" process. This is again explained in the technical specification, but
  not strictly needed to implement the effect creation and execution process.

The source code of these scripts may be found under the `agora-drep` directory.

### The intended use of the scripts

As explained in the Agora documentation and the technical specifications, the voting
script, proxy script and PlutusV3 version of the GAT minting policy are designed
to be used in tandem.

The intended use of the scripts pre-suppose the following:

* _An Agora-based governance system exists_, allowing a set of actors to
  propose actions (implemented as Agora effects) and joinly choose to execute them
  or reject them (via any mechanism or policy of their choice).

* _The participants of this Agora system choose to delegate some or all of their
  ADA to the automated DRep_.

With these two pre-requisites in mind, the scripts may be used in the following way:

1. **DRep creation**: the automated DRep is created in the same way a human DRep is registered,
   with the difference that the DRep voting credential used is the **credential that corresponds
   to the voting script**.

2. **Fund delegation to the DRep**: users of the Agora governance system can delegate their
   funds to the automated DRep. A treasury may be used (like Clarity's Agora DAOs
   do) or delegation may be achieved through other mechanisms.

3. **Cardano governance proposal vote**: users of the Agora system can make the automated
   DRep vote on a Cardano governance proposal by performing the following steps:

    1. Create a Proxy effect, containing a Voting effect inside (proxy effects function as
       wrappers). The voting effect must refer to the governance action to vote in and
       the specific vote to cast (Yes, No, Abstain). Consult the Haddock documentation for
       more information on the format of the Proxy and Voting datums.

    2. Choose whether to execute the proxy-voting effect using the appropriate mechanisms
       the Agora system has. For example, Clarity's Agora DAOs users vote by using a specific
       currency.

    3. Execute the proxy effect. As a result of the previous step, an effect
       UTxO holding a GAT should exist. Furthermore, the UTxO should have as a
       datum the proxy-voting effect the Agora system has chosen.   

    4. Execute the voting effect. As a result of the previous step, an effect UTxO
       holding a new GAT should exist. The UTxO should have as a datum the voting
       effect chosen.
       After the execution of this last effect, the actual voting on the Cardano
       governance proposal will have been performed by the automated DRep.

## Setup and deployment

### Serialization of the scripts

All scripts can be serialized by running the export executable:

```bash
$ cabal run agora-drep-script-export
```

This will create a `scripts` folder (if one does not exist already). Therein the
scripts for the new proxy and voting effects may be found in binary format:

```bash
$ tree scripts
scripts/
└── agora
    ├── proxyScript.bin
    ├── proxyScript.debug.bin
    ├── votingEffectScript.bin
    └── votingEffectScript.debug.bin
````

### Script instantiation / applying arguments

Assuming an Agora system already exists, the handling of the new Proxy and Voting
effects is not different from other Agora effects.

Consult the Haddock documentation of the [Proxy effect](https://mlabs-haskell.github.io/agora-drep/haddock/Agora-Proxy.html) and [Voting effect](https://mlabs-haskell.github.io/agora-drep/haddock/Agora-Effect-Voting.html)
to see the function signature of both effects.

Specifically, both effects (like all Agora effects) are parameterized by
the `CurrencySymbol` of the enabling GAT, which will depend on the specific
Agora system in use. Therefore, the scripts **must** have this argument applied
before being used.

### Creating an effect UTxO

For effect creation, consult the definition of the datum types of the scripts:

* [ProxyDatum](https://mlabs-haskell.github.io/agora-drep/haddock/Agora-Proxy.html#t:ProxyDatum)
* [VotingDatum](https://mlabs-haskell.github.io/agora-drep/haddock/Agora-Effect-Voting.html#t:VotingDatum)

For creating a voting effect, the following requirements must be met:

* A UTxO **must** be created at the address of the desired effect.
* The UTxO **must** contain a valid datum according the data definitions linked previously
  (i.e: the datum must be of type `VotingDatum`).
* The UTxO **must** contain.

Any failure to comply with these will make the UTxO unspendable.

### Executing the effect

For a detailed guide on how effects are executed on Agora based systems, consult
the following Notion documentation:

* [Effects](https://liqwid.notion.site/Effects-3364731305544dc1972221b6d1353a69)
* [Authority Tokens](https://liqwid.notion.site/Authority-Tokens-b25d2011c8114e04ac9e73514e6b9421)

The voting script cannot be executed directly by a typical Agora system due to
PlutusV2/PlutusV3 incompatibilities. This means a more involved process is required
for creation and execution of the effect.

In a previous [section](#the-intended-use-of-the-scripts), this process is sketched.

Again, for more information on this, consult the technical specification, where
specific interactions between the Voting, Proxy and Minting scripts are thoroughly
documented.

However, for the purposes of implementing effect creation and execution, any already
exiting Agora based system should be able to support the new voting effect:

* The voting effect creation works exactly the same (just the datum and effect address
  are different)
* The voting effect execution works similarly, but now two executions are required: one
  for the proxy effect, and another for the proper voting effect.

## Examples

> Use case: an Agora DAO delegates the ADA contained in its treasury to an automated DRep,
> which can subsequently vote on Cardano governance proposals **when and only when** the
> DAO members wish it to do so.

This use case is already implemented in [Clarity](www.clarity.vote), but let's see
how it can be achieved with the work provided here:

* Agora DAOs have treasuries, with a specific associated payment and staking address.

* Upon creation, Agora DAOs may choose to delegate all ADA contained in the treasury
  address to an automated DRep. The DRep credential is derived from the voting script
  contained in this repository.

* The voting script is parameterized by a GAT which can only be minted when the DAO
  agrees by majority to execute an on-chain effect.

* Any DAO member may create proposals (which are different from Cardano governance proposals!)
  that, when passed, will create a Proxy-Voting effect that can be executed.

* Any Cardano user may execute the effect after it was created. When the effect
  is executed, the automated DRep votes as the effect specifies.

This is a very elaborate use case, but simpler ones are also possible.

> Use case: a group of users decide to pool their ADA in a single, multi-signature address.
> They collectively decide to delegate the ADA in the address to an automated DRep, which
> will vote as they wish.

Let's see how this can work:

* Alice, Bob and Adam pool their ADA in a single, multi-signature address. Funds
  under this address cannot be spent or delegated without (at least) 2 out of 3
  signatures. 

* They all choose to delegate the ADA under this address to an automated DRep,
  whose credential is derived from the voting script.

* The voting script is parameterized by a GAT with a currency symbol corresponding
  to _another_ multi-signature script. This GAT cannot be minted without (at least)
  2 out of 3 signatures.

* Alice wishes the DRep to vote 'Yes' on a recent Cardano governance proposal, so she
  builds a transaction that:

  * Creates a voting effect UTxO with the desired vote outcome (i.e: cast 'Yes' on
    the desired proposal).
  * Mints the required GAT and places it in the same UTxO. This is required to actually
    execute the voting effect.

* Alice pays the fees out of her own pocket, so there is no need to spend from the
  multi-signature address that she shares with Bob and Adam.

  But since she still wants to mint the GAT, and the GAT requires 2 out of 3 signatures,
  she needs someone else to sign the transaction too. Fortunately, she convinced Bob
  too, so she sends the partially signed transaction to Bob to meet the minimum threshold
  of 2 signatures, and finally Bob submits the transaction.

* Because Alice already paid for the fees of the effect creation, Bob decides to pay for the
  effect execution. He builds a transaction that spends the effect UTxO and also
  submits the desired 'Yes' vote on the Cardano governance proposal. He will also require
  the signature of Alice (or Adam), because the GAT must be burnt. Once the TX is fully
  signed, Alice submits the final transaction and the process ends with a successful
  Cardano governance vote.

This is a simpler use case than the previous one, because a full DAO is not required and
all the logic is implemented with Cardano native scripts.

## References

Please consult the following documents for more information about specific topics:

- For a full, technical specification of the developed scripts: [Specification](./specification)
- For the documentation of the API: [Haddock documentation](https://mlabs-haskell.github.io/agora-drep/haddock/)
- For more information about the Agora system: [Agora repository](https://github.com/Liqwid-Labs/agora)
- For a specificaction of the Agora system: [Agora specification](https://liqwid.notion.site/Agora-Specs-Overview-fd7df78313cf4dc0b1522cb9260b77d1)
- For more information on how to use an Agora DAO as a DRep: [Clarity's "Agora DAO as a DRep"](https://docs.clarity.vote/clarity-v1/agora-dao-as-a-drep)
- [Catalyst Closeout report](./closeout-report.pdf)
- [Catalyst Closeout video](https://youtu.be/2RtffHLBsZ8)
