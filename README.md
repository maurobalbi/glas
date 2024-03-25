### Disclaimer
Glas is a pet project of mine and is expected to be surpassed in terms of scope and features by the official Gleam-LSP in due time. Bug reports are much appreciated, but for feature requests and contributions please refer to the official [Gleam reposository](https://github.com/gleam-lang/gleam) instead.

# glas: Gleam Language Server 

A language server for the [Gleam](https://gleam.run/) programming language. Glas tries it's best supporting the user writing gleam, even in the presence of parsing and typechecking errors!

## Installation

Currently MacOS (arm64, x86), Linux (x86), Windows (x86) are supported. Other targets have to be compiled from source.

### VSCode

Install the extension from [VisualStudio Marketplace](https://marketplace.visualstudio.com/items?itemName=maurobalbi.glas-vscode). Binaries are included in the extension archive.

### NeoVim

```lua
require'lspconfig'.gleam.setup{
  cmd = { "glas", "--stdio" }
}
```

## Features

- Resilient Parsing and Type Checking
- Completions
- Goto Definition
- Find References
- Highlight Related
- Hover Info
- Syntax Errors
- Semantic Highlighting
- Show Syntax Tree
- Rename

### Motivation?

This project was primarily motivated by my personal learning about how language servers, in general, and rust-analyzer, in particular, work. Gleam caught my interest due to it's lovely and active community and the interesting niche (functional, statically typed, simple) it's trying to fill. 

### Goals
- Personal Learning
- Good UX
- Provide only correct refactorings (if it compiles before it compiles after)
- Robustness

### Non goals
- Aiming for 100% diagnostics (at least short / midterm)
- Code generation

### Architecture

Since the code in an actual file during development is usually in a `broken` state, having a specialized IDE architecture helps making sense of the code on a best-effort basis.

In a broad sense, the architecture of glas can be thought of as going from chained steps of ```fn(IR) -> Result(IRn, Err)``` to  ```fn(IR) -> (IRn, List(Err))```

Where this is particularly evident is in the parser. Instead of aborting at the first error, the parser always produces a concrete syntax tree and collects errors as it goes. [This blog post by matklad](https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html) provides an excellent explanation of this concept, and the parser in this project is based on the ideas presented there. To visualize the syntax tree of a file, the VSCode extension implements the "show syntax tree" command, allowing you to explore the tree as you type.

## Acknowledgments

This project is heavily inspired by the exellent [Nix language server](https://github.com/oxalica/nil) and [rust-analyzer](https://github.com/rust-lang/rust-analyzer).
