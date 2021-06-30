---
layout: post
title: Simple Verifiable Elections
description: How cryptography can increase both security and confidence in our elections.
tags: [crypto]
mathjax: true
citation: true
---

## Introduction

On November 3, 2020, we witnessed the "most secure election in US
history" [[^CISA]], yet 60% of Republican voters distrust the election
system [[^MorningConsult]]. In this paper, I will discuss how we can leverage
cryptography to increase both security and confidence in our elections.
Specifically, I will describe an election protocol developed by Josh
Benaloh that aims to be simple while still achieving end-to-end
verifiability [[^Benaloh]].

### Definitions

End-to-end verifiability (E2E) provides integrity to an election by allowing
voters to audit the information published by the system, rather than trusting
that the system has behaved correctly. Specifically, voters can verify that
their votes are both cast as intended (*individually verifiable*) and tallied
as cast (*universally verifiable*). However, votes should
also be kept private, to avoid such problems as voter coercion and bribery. This
privacy aspect is where the difficulty arises; without this constraint, an
election system could just post the entirety of votes on a public bulletin and
satisfy the first two constraints.

It's also worth noting here that the current US election system doesn't provide
full privacy, specifically in the case of provisional ballots. We will see
that the system  described below does maintain secrecy for all but
*illegitimate* provisional ballots, which is a significant improvement.

## Encryption

To protect voter privacy, voters cast encrypted ballots. Once encrypted, the
ballots can be viewed by the public. However, a simple public-key encryption
scheme where a single entity in possession of the secret key can perform
decryption would result in a single point of potential privacy compromise.
Instead, we use *threshold encryption*, for which decryption requires
a predetermined minimum number of *trustees* to work together.
Furthermore, as will be detailed later, to securely tally the encrypted ballots
this system will rely on the ability for users with the public key to perform
re-encryptions. This election system does not prescribe a particular version of
encryption, but as outlined in [[^Benaloh]]
we will describe a scheme based on ElGamal threshold encryption that supports
re-encryption [[^ElGamal]].

### ElGamal Encryption

First we present the ordinary single-entity public-key ElGamal encryption
scheme. As in Diffie-Hellman, we first agree on a large prime \\(p\\) and generator
\\(g\\) of \\(\mathbb{Z}_p\\). All operations are done modulo \\(p\\). Anyone can choose
some secret key \\(s\\) and corresponding public key \\(z = g^s\\). A message \\(M \in
\\{0\ldots,p-1\\}\\) is then randomly encrypted by choosing a random integer \\(r\\)
such that \\(0 < r < p\\) and forming the pair
\\[ (x, y) = (M z^{r}, g^r). \\]
To decrypt with the secret key \\(s\\), one just computes \\(\frac{x}{y^s}:\\)

\begin{align}
  \frac{x}{y^s} \ &= \ \frac{ M z^r}{(g^r)^s} \\\\\
                \ &= \ \frac{M (g^s)^r}{(g^r)^s}  \\\\\
                \ &= \ M
\end{align}

### Re-encryption

Next, anyone can randomly re-encrypt this \\((x, y)\\) pair by selecting another
\\(r'\\) such that \\(0 < r' < p\\), and forming the new pair
\\[ (x', y') = (xz^{r'}, yg^{r'}).\\]
The original message can be decrypted using the same method:
\begin{align}
  \frac{x'}{(y')^s} \ &= \ \frac{ xz^{r'}}{(yg^{r'})^s} \\\\\
                    \ &= \ \frac{Mz^rz^{r'}}{(g^rg^{r'})^s}   \\\\\
                    \ &= \ \frac{Mz^{r+r'}}{g^{(r+r')s}}   \\\\\
                    \ &= \ \frac{M(g^s)^{r+r'}}{g^{(r+r')s}}   \\\\\
                    \ &= \ \frac{Mg^{(r+r')s}}{g^{(r+r')s}}   \\\\\
                    \ &= \ M
\end{align}
Also, under this re-encryption scheme, the Diffie-Hellman assumption guarantees
that an observer does not know that \\((x,y)\\)
and \\((x',y')\\) decrypt to the same message \\(M\\) unless they are given the value
\\(r'\\), in which case they can just check that \\((x', y') = (xz^{r'}, yg^{r'})\\).

### Threshold Encryption

Next we employ Shamir's threshold scheme [[^Shamir]] to allow an arbitrary
number of shareholders to coordinate a decryption. Again let \\(s\\) denote the
secret key. Suppose we want to impose a threshold of \\(k\\) shareholders required
to decrypt. Let
\\[ P(x) = s + \sum_{i = 1}^{k-1}  a_{i}x^{i} \\]
be a random polynomial, where \\(a_i\\) are random integers such that \\(0 < a_i < p\\),
and \\(s\\) is the secret key. Then \\(n\\) parties \\(\mathcal{P}_1 , \ldots,
\mathcal{P}_n\\) each hold a point \\(P(i)\\) on this polynomial for some \\(n \ge k\\).
Then as long as \\(k\\) of them coordinate and share their \\(k\\) points, the
polynomial of degree \\(k-1\\) can be interpolated to discover \\(s = P(0)\\).

## Ballot Casting

The question remains how to accomplish ballot casting from a procedural
perspective. For example, if voters go to a polling station and use
official election equipment, how can they trust that the ballots are actually
cast as intended? A simple solution is to have "vote creation devices" where
voters enter their ballot selections, and the device does nothing but produce
the encrypted selections; for example, the device could print out a
swipeable card with a magnetic stripe recording the encrypted ballot, with a
hash of the encrypted ballot printed on the card. Then a voter can take their
card to a poll worker, give their identification and swipe their card to cast
their vote. Then the voter can take the card with them to later verify that
their vote is included in the final published election tally.

## Tallying

The main idea behind tallying a set of encrypted ballots while maintaining
voter privacy is to

1. first strip identifying information from each ballot,
2. then interested parties are invited to repeatedly re-encrypt and shuffle the set,
3. and finally, a number of trustees meeting the threshold decrypt the shuffling.

### Shuffling

We will spend most of this section describing step 2.
We want to verify that this shuffling process results in the same tally as the
original set, while obscuring the exact equivalence between votes (so as not to
leak ordering information from the initial ballot set).

Let \\(\mathcal{B}\\) be a set of ballots encrypted with the method described
[above](#encryption). Let \\(\pi\\) denote a random permutation and \\(R\\)
denote a random re-encryption operation (which does not require a secret key). Then let
\\(\mathcal{B}' \\) be the set resulting from re-encrypting each ballot in
\\(\mathcal{B}\\) and shuffling the set:
\begin{align}
  \mathcal{B}'
    \ \xleftarrow{\$} \ \pi \\left\\{ B' \xleftarrow{\$} R(B) : B \in \mathcal{B}\\right\\}
\end{align}
We then perform an interactive proof that \\(\mathcal{B}\\) and \\(\mathcal{B}'\\) are
equivalent (with high probability), without revealing the exact permutation
details. The proof goes as follows:

Just as we re-encrypted and permuted \\(\mathcal{B}\\) to create \\(\mathcal{B}'\\), do
this \\(n\\) more times to create encrypted ballot sets \\(\mathcal{B}_1,\ldots,
\mathcal{B}_n\\). Then generate \\(c_1,\ldots c_n\\) challenge bits (described in the
following [section](#fiat-shamir-heuristic)).

For each \\(i\\) such that \\(c_i = 0\\): reveal the re-encryption and permutation data
used to create \\(\mathcal{B}_i\\) to show that it is equivalent to \\(\mathcal{B}\\).

For each \\(i\\) such that \\(c_i = 1\\): compose the re-encryption and permutation data
used to create \\(\mathcal{B}_i\\) with that used to create \\(\mathcal{B}'\\), then
reveal the composition to show that it is equivalent to \\(\mathcal{B}'\\).

Using this method, we never reveal the exact equivalence relation between
\\(\mathcal{B}\\) and \\(\mathcal{B}'\\), so the shuffle completely obscures any
ordering information from the ballot gathering phase. The interactive proof only
fails if the prover can guess the \\(n\\) challenge bits and make sure that all
the sets in \\(\\{\mathcal{B}_i: c_i = 0\\}\\) are shuffles of the original ballot set
and all the sets in \\(\\{ \mathcal{B}_i: c_i = 1\\}\\) are shuffles of some other
(adversarial-intended) set. This happens with probability \\(2^{-n}\\), so a
reasonable \\(n\\) can be chosen to ensure sufficient security.


### Fiat-Shamir Heuristic

Because the security of the shuffle hinges on the verifier not being able to
guess the challenge bits ahead of time, particular care is needed in how they
are generated. As detailed in [[^Benaloh], [^Shamir]], we can use the
Fiat-Shamir Heuristic to generate challenge bits within the interactive proof
itself: use a one-way hash function \\(H: \\{0, 1\\}* \to \\{0,1\\}^n\\) to generate
\\[ c_1 \cdots c_n \ \leftarrow \ H( \mathcal{B}_1,\ldots, \mathcal{B}_n) \\]
Now an adversary cannot alter any of the ballot sets to get passed the challenge
bits; any alteration of the sets will produce an entirely different challenge.

### Publishing

At the end of this process, election officials can take all the encrypted
ballots and publish them on a public site. Because they are encrypted, they can
even be posted with voter names attached. Each shuffling and proof of shuffling
is also published.  Finally, after all interested parties have finished
shuffling, a set of trustees meeting the threshold decrypt the final encrypted
ballot set. Anyone who wishes to do so can use this public data to verify
each step was executed correctly, and voters can check that their encrypted
ballot is included in the original ballot set; however, voters cannot identify
their decrypted ballot.

## Auditing

There are a number of ways to incorporate auditing into the voting process. Here
we describe one of the simplest, which is to add decryption available
at the vote creation devices.

Consider the ballot casting described [above](#ballot-casting). Recall
that voters keep their encrypted ballot card and, when the
encrypted votes are later published, they can verify that their ballot is
included in the  final tally. However, we still need a way for voters to verify
that their ballot selections were actually cast as intended. To this end, extend the
simple vote creation device in the following way: after the encrypted ballot is
created, the voter is presented with the question, "Do you wish to cast this vote?".
If the voter answers "Yes", the ballot is digitally signed with an attestation
of the legitimacy of the ballot, which is
now *required* to be cast. If the voter answers "No", the encrypted ballot
does not get the required digital signature, and thus cannot be cast,
but the machine also prints verifiable decryption information so that the voter
can later verify that this uncasted ballot was encrypted as intended.

The reason we cannot provide both a legitimacy attestation and decryption
information is that we don't want voters to have a *receipt* of their
ballot selections; election systems that have such receipts are subject to voter
coercion. The "No" option allows anyone to be an *observer* of the
process to ensure election accuracy, and this includes normal voters. Once a
voter selects "No" they can choose to change their selections, or keep them the
same, and a new encrypted ballot will be printed with the same question posed.
Of course, since the encryption is randomized, there will be no link from a
prior illegitimate ballot to a later legitimate ballot. Because the encrypted
ballot is printed *before* the question prompt, the system has no way to
know whether or not its encryption will be audited, so there's no way to fool
the audit process, and voters can be confident that if their
illegitimate ballots decrypt properly, then their legitimate ballot does as well.

## Conclusion

During this lame duck period of the 2020 US Election, it has become abundantly
clear that we need more trust in our election systems. Cryptographic E2E systems
such as the simple one presented here will allow us to ensure security
*and* restore trust in the process. In fact, one such E2E system
Scantegrity II was deployed for a local election in Maryland in 2010-2011 and was
largely viewed as a success [[^md]]. Another promising system
currently under development is ElectionGuard, an open source project which was jointly
designed by Josh Benaloh and the engineering staff at Galois, Inc [[^ny]].
While the simple system described above uses shuffling (also known as a
*mix-net*) to tally encrypted ballots, the ElectionGuard system leverages
*homomorphic encryption* to tally encrypted ballots, e.g. \\(Enc(v_1 +
v_2) = Enc(v_1) + Enc(v_2)\\). ElectionGuard had its first trial run during the
2020 US primaries in Fulton, Wisconsin [[^wi]] and is expected to roll out to
other parts of the country in the coming years.

## References

[^CISA]: GCC, CISA, NASS, NASED, SCC, Democracy Works, et al.  [Joint Statement from Elections Infrastructure Government Coordinating Council & the Election Infrastructure Sector Coordinating Executive Committees](https://www.cisa.gov/news/2020/11/12/joint-statement-elections-infrastructure-government-coordinating-council-election): (2020).
[^MorningConsult]: Laughlin, Nick and Shelburne, Peyton. [How Voters' Trust in Elections Shifted in Response to Biden's Victory]( https://morningconsult.com/form/tracking-voter-trust-in-elections): (2020).
[^Benaloh]: Benaloh, Josh. "Simple Verifiable Elections." *EVT 6*: (2006).
[^ElGamal]: ElGamal, Taher. "A Public-Key Cryptosystem and Signature Scheme Based on Discrete Logarithms." *IEEE Transactions on Information Theory*: (1985).
[^Shamir]: Shamir, Adi. "How to Share a Secret." *Communications of the ACM 22*: (1979).
[^Fiat]: Fiat, Amos and Shamir, Adi. "How to Prove Yourself: Practical Solutions to Identification and Signature Problems." *Proceedings of Crypto '86*: (1986).
[^md]: Richard Carback, David Chaum, Jeremy Clark, John Conway, Aleksander Essex, Paul S. Herrnson, Travis Mayberry, Stefan Popoveniuc, Ronald L. Rivest, Emily Shen, Alan T. Sherman, and Poorvi L. Vora. "Scantegrity II municipal election at Takoma Park: the first E2E binding governmental election with ballot privacy." *Proceedings of the 19th USENIX conference on Security (USENIX Security'10)*: (2010).
[^ny]: Halpern, Sue. [Can Our Ballots Be Both Secret And Secure?](https://www.newyorker.com/news/the-future-of-democracy/can-our-ballots-be-both-secret-and-secure) *The New Yorker*: (2020).
[^wi]: Burt, Tom. [Another step in testing ElectionGuard](https://blogs.microsoft.com/on-the-issues/2020/02/17/wisconsin-electionguard-polls/) *Microsoft On The Issues*: (2020)
