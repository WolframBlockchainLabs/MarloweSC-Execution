# Response to Audit Report for the Wolfram Oracle

## 1. Executive summary

## 2. Acknowledgment

We extend our gratitude to the High Assurance Software Group at Tweag for their review of the Wolfram Oracle Validator. Their findings, suggestions, and open communication have been key in enhancing this product.

## 3. Overview of Audit report

The Tweag team conducted an analysis on the Wolfram Oracle Validator, focusing on the file `OracleValidator.hs`. They made minor adjustments on the codebase to address dependency mismatches.

The Audit was based on documentation and discutions with the Wolfram team and performed with the `cooked-validators` library for testing.

They organized their findings in three categories (Low, Medium, High) depending on the severity. In total, they report 14 issues. The report includes remediation measures for each issue. Additionally, they delivered a nix enviroment, a test suite and an example of a fixed validator.

## 4. Overview of changes to Oracle Validator

The audit report's main concerns for the Oracle Validator were categorized as follows: 

* Logical flaws
* Code quality issues
* Design flaws
* Unclear specifications

Based on the Audit's report, the primary changes on the Oracle Validator include the following.

* The validator now uses a redeemer to specify which validations should be performed (`Execute` or `Reclaim`)
* The validator now performs explicit time interval and signature checks.
* The error message system has been updated to provide more informative feedback.
* The deserialization of objects has been optimized to reduce resource consumption during execution.

The following section details the specific changes made, along with the commits that implement them.

## 5. Responses to specific findings

### 5.1 Incorrect validity interval check

*Severity: Critical*

**Finding:** 

The validator performs weak time interval checking. For the *Reclaim* case, the line

```haskell
contains (from (limitTime + 1)) txValidRange
```

directly checks the condition that the transaction is happening after the deadline. For the *Execute* case, the condition is the negation of the above line which can lead to undesired behavior. 

**Solution:** 

The commit [**3df5c4c**](https://github.com/WolframBlockchainLabs/MarloweSC-Execution/commit/3df5c4c9a2b286583110c514d800c9cb29e871e8) branches the validator’s execution based on a new redeemer structure and the commit [**9064a01**](https://github.com/WolframBlockchainLabs/MarloweSC-Execution/commit/9064a0189f5adf1187f5c9b9701e9ba76282a639) adds the logic expected for each redeemer. Now, for each case, the validator directly checks whether the validity interval falls within the proper domain:

- $]-\infty, deadline]$ for *Execute*
- $]deadline, +\infty[$ for *Reclaim*

### 5.2 Incorrect top-level exclusive disjunction condition

*Severity: Critical*

**Finding:** 

The validator determines which action is being performed by checking conditions that are intended to be exclusive. The conditions are as follows:

- (*A*) The redeemer of the Marlowe contract input has the correct shape and its *ChoiceId* matches the choice name in the oracle’s datum.
- (*B*) The validity interval of the transaction is contained within $]deadline, +\infty[$
- (*C*) The beneficiary after the deadline has signed the transaction

The table of truth in page 6 of the audit report shows that there are cases where success occurs when a failure is expected.

**Solution:** 

To resolve the finding, a new redeemer structure is introduced. The branching ensures only positive checks are performed for each case.

- For the *Execute* case, the validator checks the condition (*A*) and that the execution occurs before the deadline.
- For the *Reclaim* case, the validator checks conditions (*B) and (C*).

Both fixes are included in commits [**3df5c4c**](https://www.notion.so/Response-to-Audit-Report-for-the-Wolfram-Oracle-18b6ed77d0c08047ac61e1b6679bbae6?pvs=21) and [**9064a01**](https://github.com/WolframBlockchainLabs/MarloweSC-Execution/commit/9064a0189f5adf1187f5c9b9701e9ba76282a639)

### 5.3 Lack of Wolfram wallet in the codebase

*Severity: High*

**Finding:**

As part of the specification, when the Oracle provides the requested data, it should also collect the funds into the designated *Wolfram wallet.* The audit highlights that the lack of a specific address allows any user to “execute the transaction and redirect the remaining funds to their own wallet”. This issue undermines the system's reliability and trustworthiness.

**Solution:**

To address this issue, a *wolframStaticWallet* will be integrated into the codebase to ensure that the signer of the *Execution* transaction is always an authorized address. It is important to note that any modification to this address will also alter the address of the Oracle Validator. As a result, all changes or updates will be thoroughly documented and made publicly accessible to users.

The commit [**a7fdc55**](https://github.com/WolframBlockchainLabs/MarloweSC-Execution/commit/a7fdc55a89583c3ccdb08eee9c480696e3d4b7ea) includes the changes mentioned.

### 5.4 Incomplete pattern matching on the Marlowe redeemer

*Severity: Medium*

**Finding:**

The Oracle Validator performs non-exhaustive pattern matching which could lead to unexpected behavior. Specifically, the validator expects a list of one *Marlowe redeemer* but does not handle cases when the redeemer is an empty list or contains two or more elements. 

**Solution:**

The commit  [**9064a01**](https://github.com/WolframBlockchainLabs/MarloweSC-Execution/commit/9064a0189f5adf1187f5c9b9701e9ba76282a639) includes a complete pattern matching for the *Marlowe Redeemer*. 

### 5.5 Unchecked warnings and lack of coding tooling

*Severity: Medium*

**Finding:**

The audit identified warning and formatting inconsistencies during the code review. As they mention, no standard Haskell tooling was used.

**Solution:**

The development environment has been configured to use the *Haskell Language Server* and the *Ormolu* formatter to ensure better practices during development.

### 5.6 Unnecessary copy-paste of the Marlowe codebase

*Severity: Medium*

**Finding:**

A file called *Types.hs* is a copy-pasted version of the datatypes used by Marlowe. The only data structure utilized is the `Input` type which servers as the redeemer for the Marlowe input. The inclusion of this file unnecessarily increases the size of the validator.

**Solution:**

A new file names *MinimalTypes.hs* replaces *Types.hs.* Now, only the `Input`structure is included. This change was implemented in commit [**60548e1**](https://github.com/WolframBlockchainLabs/MarloweSC-Execution/commit/60548e1e348558a54e11ebace1c3e4b72d67c31e).

### 5.7 Lack of deserialization optimization

*Severity: Medium*

**Finding:**

The Oracle Validator deserializes every element it uses, including the Oracle datum, redeemer, the Marlowe redeemer and script context, even though it only requires some elements from these data structures. This leads in excessive resource consumption, which can be reduced.

**Solution:**

The commit [**9064a01**](https://github.com/WolframBlockchainLabs/MarloweSC-Execution/commit/9064a0189f5adf1187f5c9b9701e9ba76282a639) implements the views recommended by the audit team.

### 5.8 Improvable variable names

*Severity: Low*

**Finding:**

The Oracle Validator uses non-meaningful variable names, making the code difficult to review and follow.

**Solution:**

Commit [**3b3ccb0**](https://github.com/WolframBlockchainLabs/MarloweSC-Execution/commit/3b3ccb0d2ced26b8cecc0b22f478fe06e91bbb61) updates the variable names based on the audit team’s suggestions.

### 5.9 Too many variables in the `where` clause

*Severity Low*

**Finding:** 

The Oracle Validator introduces an excessive number of variables derived from data structures such as datums, redeemers, and the script context. This can be avoided by using standard Haskell techniques.

**Solution:**

Commit [**9064a01**](https://github.com/WolframBlockchainLabs/MarloweSC-Execution/commit/9064a0189f5adf1187f5c9b9701e9ba76282a639) implements the destructuring of variables into arguments, as recommended by the audit team.

### 5.10 Incomplete error messages

*Severity: Low*

**Finding:**

The Oracle Validator lacks meaningful error messages for most of failure cases, making it difficult to identify what went wrong.

**Solution:**

Commit [**9064a01**](https://github.com/WolframBlockchainLabs/MarloweSC-Execution/commit/9064a0189f5adf1187f5c9b9701e9ba76282a639) adds error messages as suggested by the audit team.

### 5.11 Outdated dependencies

*Severity Low*

**Finding:**

The Wolfram Oracle dependencies on Plutus and Cardano APIs are outdated and can be upgraded. This is particularly important since the `plutus-ledger-api` has a new version available. The audit team recommends keeping the dependencies up to date.

**Solution:**

This recommendation requires further discussion within the Wolfram team, specially since the Oracle Harvester infrastructure currently only supports up to PlutusV2 contracts. An upgrade on this infrastructure will be implemented in subsequent development phases.

### 5.12 Oracle datum is not required to be inlined

*Severity: Low*

**Finding:**

The audit team highlights the possibility of using a hashed datum when deploying the Oracle Validator. This is problematic because it forces users to share the datum off-chain. Additionally, the team emphasizes that oracles should ensure data transparency and accessibility.

**Solution:**

Since enforcing the use of an inline datum at execution time would require “significant changes” to the validator, we opted to thoroughly document the implications of using hashed, embedded, and inline datums, as well as the outcomes of each approach. This documentation will be made publicly available for users.

### 5.13 Leftover of dead code

*Severity: Lowest*

**Finding:**

Some files contain commented-out code from older versions of the Oracle Validator and related functions.

**Solution:**

Commit [**76d8cfe**](https://github.com/WolframBlockchainLabs/MarloweSC-Execution/commit/76d8cfeeccd1714407d150d4a0c94a566ffbe6a0) removes unnecessary comments, resulting in cleaner and more maintainable files.

### 5.14 Unnecessary unfolding of the `TxOutRef` content in the UTXODatum

*Severity Lowest*

**Finding:**

The Oracle datum unfolds the `TxOutRef` into two primitive elements (*transactionId* and *transactionIndex)* but uses them together to identify the MarloweUTXO.

**Solution:**

The commit [**5de3c5b**](https://github.com/WolframBlockchainLabs/MarloweSC-Execution/commit/5de3c5b1507e0460613d9c3f84246c575d239c20) implements a method to store the `TxOutRef` directly, following the model provided by the audit team.