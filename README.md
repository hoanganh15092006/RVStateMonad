<div align="center">

# ⚙️ RVStateMonad

### *A Clean & Performant State Monad Implementation for Functional Architecture*

[![GitHub License](https://img.shields.io/github/license/hoanganh15092006/RVStateMonad?style=for-the-badge&color=blue)](LICENSE)
[![Build Status](https://img.shields.io/github/actions/workflow/status/hoanganh15092006/RVStateMonad/ci.yml?branch=main&style=for-the-badge)](https://github.com/hoanganh15092006/RVStateMonad/actions)
[![Language](https://img.shields.io/badge/Language-Haskell%20%2F%20Rust-orange?style=for-the-badge)](https://github.com/hoanganh15092006/RVStateMonad)
[![Cardano Ecosystem](https://img.shields.io/badge/Ecosystem-Cardano-0033AD?style=for-the-badge&logo=cardano&logoColor=white)](https://cardano.org)

</div>

---

## 📖 Overview

**RVStateMonad** is a lightweight, purely functional **State Monad** library designed to simplify stateful computations in complex workflows. It encapsulates state transformations, avoiding global mutable state while maintaining deterministic, testable, and composable execution paths.

This library is particularly suited for:
- ⛓️ **Cardano / eUTXO Off-chain & On-chain logic** requiring deterministic state transitions.
- 🔄 **Functional Pipelines & DSLs** that process sequential transformations.
- 🧪 **Testable Systems** needing isolated mock states without side effects.

---

## ✨ Key Features

- 🎯 **Purely Functional**: Zero side-effects; state mutations are modeled as explicit transitions `s -> (a, s)`.
- ⚡ **Composable**: Seamlessly chain stateful operations using monadic binding (`>>=`, `bind`, or `do-notation`).
- 🛡️ **Type-Safe**: Prevents illegal state transitions at compile time.
- 📦 **Zero External Dependencies**: Clean and easy to integrate into existing codebases.

---

## 🏗️ Architecture & Concepts

The core concept relies on the State Monad pattern:

$$\text{State } s \ a = S \rightarrow (a, S)$$

Where:
- $S$: The type of the state being tracked/modified.
- $A$: The result value produced by the computation.

### Monadic Operations Supported

| Function / Operation | Description |
| :--- | :--- |
| `get` | Fetch the current state from the monad context. |
| `put` | Replace the internal state with a new state. |
| `modify` | Transform the state using a pure function `(s -> s)`. |
| `runState` | Execute the monad with an initial state and return `(result, finalState)`. |
| `evalState` | Execute the monad and return only the `result`. |
| `execState` | Execute the monad and return only the `finalState`. |

---

## 🚀 Getting Started

### Prerequisites

Ensure you have the required toolchain installed:
- **GHC / Cabal** (if Haskell) or **Cargo / Rust** (if Rust implementation).

### Installation

Clone the repository to your local environment:

```bash
git clone [https://github.com/hoanganh15092006/RVStateMonad.git](https://github.com/hoanganh15092006/RVStateMonad.git)
cd RVStateMonad
