### Disclaimer
Glas is a pet-project of mine and expected to be eventually surpassed in scope and features by the official Gleam-LSP. Bug reports are very welcome, but for feature-requests and other contributions please refer to the official [gleam-repo](https://github.com/gleam-lang/gleam) instead.

# glas: Gleam Language Server 

A language server for the [Gleam](https://gleam.run/) programming language.

## Installation

Currently MacOS (arm64, x86), Linux (x86), Windows (x86) are supported. Other targets have to be compiled from source.

### VSCode

Install the extension from [VisualStudio Marketplace](https://marketplace.visualstudio.com/items?itemName=maurobalbi.glas-vscode). Binaries are included in the extension archive.

### NeoVim lspconfig

```lua
require'lspconfig'.gleam.setup{
  cmd = { "glas", "--stdio" }
}
```

### Others

For other platforms binaries have to be built from source.

## Features

- Resilient parsing
- Completions
- Goto-Definition
- References
- Highlight-Related
- Hover-Info
- Syntax-Errors
- Semantic highlighting
- Show syntax-tree
- Rename

### Motivation?

This project was primarily motivated by my personal learning about how language servers, in general, and rust-analyzer, in particular, work. Gleam caught my interest due to it's lovely and active community and the interesting niche (functional, statically typed, simple) it's trying to fill. 

### Architecture

Since the code in an actual file during development is usually in a `broken` state, having a specialized IDE architecture helps making sense of the code on a best-effort basis.

In a broad sense, the architecture of glas can be thought of as going from chained steps of ```fn(IR) -> Result(IRn, Err)``` to  ```fn(IR) -> (IRn, List(Err))```

Where this is particularly evident is in the parser. Instead of aborting at the first error, the parser always produces a concrete syntax tree and collects errors as it goes. [This blog post by matklad](https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html) provides an excellent explanation of this concept, and the parser in this project is based on the ideas presented there. To visualize the syntax tree of a file, the VSCode extension implements the "show syntax tree" command, allowing you to explore the tree as you type.

### Goals
- Learning
- Good UX
- Provide only correct refactorings (if it compiles before it should compile after)
- Emphasize resilience

### Non goals
- Aiming for 100% diagnostics (at least short / midterm)
- Code generation

## Acknowledgments

This project is heavily inspired by the exellent [Nix language server](https://github.com/oxalica/nil) and [rust-analyzer](https://github.com/rust-lang/rust-analyzer).
