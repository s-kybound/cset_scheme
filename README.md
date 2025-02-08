# CSET-Scheme

`CSET-Scheme` is an **experimental** implementation of the R7RS [Scheme](https://www.scheme.org/) programming language. It aims to be faithful to the original *Structure and Interpretation of Computer Programs* (SICP) book. It is a rewrite of the [`scm-slang`](https://github.com/s-kybound/scm-slang) parser and the Scheme language runner in [`js-slang`](https://github.com/source-academy/js-slang), where Scheme is currently implemented by transpiling Scheme to JavaScript and running that with the notional machine.

`CSET-Scheme` runs on a variant of the **CSE Machine**, a notional machine developed in [**Source Academy**](https://sourceacademy.org/), an online multi-language platform to teach SICP, by my peers at the National University of Singapore (NUS) and extended by myself to handle continuations, which was presented in the International Conferenence of Functional Programming 2024 (ICFP 2024) in the paper [*Beyond SICP - Design and Implementation of a Notional Machine for Scheme*](https://arxiv.org/abs/2412.01545). I have since expanded the functionality of the notional machine, expanding it to become the **CSET Machine**, which allows the machine to capture the behaviour of Scheme macros as well.

## The goal of CSET-Scheme

Having developed a machine to handle continuations and macros, why is there still work to be done? Of particular interest to me right now is to provide complete operational semantics for *all* of Scheme's behaviour within the CSET machine, including the behaviour of macros. As of February 2025, the sematics of macros are not yet well defined. But everything else (plus continuations) is!

This implementation does not need to be fast - I merely want a framework that will make implementing Scheme in the future extremely easy. Providing a full operational semantics should help with this.

We target R7RS Scheme as the specification of Scheme that this implementation will follow.

## The CSET Machine?

Within the CSET machine, evaluation of a program is defined as a transition of state to state, where each state comprises of the Control (an instruction and s-expression stack), Stash (an operand stack for data), Environment (a value environment tree) and Transformers (a macro environment tree). Better names for T are welcome.

Each transition is based on the topmost element of the Control, which dictates the transition step to take.

## Okay, why reimplement it?

The original implementation continues to exist within Source Academy, written in TypeScript. While it *works*, a lot of overhead (both cognitive and programming) is present to allow the implementation to play nice with other languages in the platform, with which it shares libraries and tools with: for example, the CSET machine on Source Academy actually interprets *JavaScript*, and Scheme is transpiled to JavaScript there!

As such, in order to more efficiently explore the CSET machine, I am temporarily bailing on the Source Academy implementation to focus on the main topic - Scheme and the CSET machine. Hopefully, concretizing the CSET machine here will let us reimplement the Source Academy version better.

I opt to rewrite it in OCaml, as my personal thought-to-program speed is higher in any ML language for me (as compared to TypeScript) due to the strong type system, and the compatibilty of such a system (an interpreter) with the functional paradigm. It's good practice as well.
