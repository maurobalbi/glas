:warning: This is still in early development!

# glas: Gleam Language Server 

A language server for the [Gleam](https://gleam.run/) programming language.

## Installation

### MacOS (arm64, x86), Linux (x86), Windows (x86) 

Install the extension from [VisualStudio Marketplace](https://marketplace.visualstudio.com/items?itemName=maurobalbi.glas-vscode)

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

### What is the motivation to develop this even though an LSP already exists integrated in the gleam compiler?

Originally, this project was primarily motivated by my personal learning about how language servers, in general, and rust-analyzer, in particular, work. I wanted to create something that could potentially benefit others as well, so instead of inventing my own language I targeted one that's already somewhat established. Gleam caught my interest due to it's lovely and active community and the interesting niche (functional, statically typed, simple) it's trying to fill. In the longterm, I believe there are advantages of a specialized language server over one that uses the traditional compiler pipeline.

Since the code in an actual file during development is usually in a `broken` state, having a specialized IDE architecture, helps making sense of the code on a best-effort basis.

In a broad sense, the architecture of glas can be thought of as going from chained steps of ```IR -> Result<IRn, Err>``` to  ```IR -> (IRn, Vec<Err>)```

Where this is particularly evident is in the parser. Instead of aborting at the first error, the parser always produces a concrete syntax tree and collects errors as it goes. [This blog post by matklad](https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html) provides an excellent explanation of this concept, and the parser in this project is based on the ideas presented there. To visualize the syntax tree of a file, the VSCode extension implements the "show syntax tree" command, allowing you to explore the tree as you type.


### Goals
- Deliver first class ux
- Provide only correct refactorings (if it compiles before it should compile after)
- Prioritize performance
- Emphasize resilience

### Non goals
- Aiming for 100% diagnostics (at least short / midterm)
- Code generation

## Acknowledgments

This project is heavily inspired by the exellent [Nix language server](https://github.com/oxalica/nil) and [rust-analyzer](https://github.com/rust-lang/rust-analyzer).
