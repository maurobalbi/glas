add load_gleam_workspace functionality
    - Download gleam dependencies via gleam deps download
    - discover workspaces (see rust-analyzer redisover workspaces)
    - set packages / is_library (see r-a projectfolders)
    - load files (see load_entry)

idea: need to intern params, to be able to map them from param to type!

Plan:
 - locate gleam.toml and register watch / load
 - on startup / change of gleam.toml run `gleam deps download` then parse manifest. 
 - get roots of libraries via manifest / packageinfo struct
 - load all gleam files / tomls into vfs by walking directories (src / test)
 - build filesets of loaded files -> set_file_source_root, set_source_root, set_package_info

alternative:
 - walk dirs directly and create packageinfo / filesets / graph

How i think Rust analyzer loads on startup

 - Parses cargo-metadata for structure
 - uses Entry struct to load all *.rs files into vfs
 - partitions the files into filesets with common sourceroot
 - adds all changes to salsa db on apply_change

ModuleInterfaces
 - Add deps field to check if module forms cycle


New query infer_definition() -> Ty
takes a definition (NameId, File) and returns a generalized type

Rust analyzer
  - FunctionId is roughly Idx<(FileId, SyntaxPtr)> in Gleam because ModuleId == FileId and no macros. in rust there really are many layers due to macros and nested modules.
  - ItemTreeNode is a trait which is implemented for syntax top level items in a module, and allows to look itself up in a item tree 
  - AstId is an interned SyntaxPtr to make it more stable
  - FileItemTreeId is a newtype around Idx<ItemTreeNode>
  - TreeId is the location in a Block and File (HirFile)
  - ItemTreeId is the combination of FileItemTree and TreeId

Refactor to 
 - ModuleData only knows about toplevel definitions Imports, Functions, etc
 - Functions can be inferred lazily when resolving a name-ref. The types need to be generalized. When resolving a type it needs to be hydrated into the environment.
 - mutually recursive functions are gonna be tricky. Idea: Pass a "stack" BTreeSet of nameids and fileid and check if own name is included. If yes infer all functions in btreeset, if fileid changes reset Btreeset. Doesnt seem very good idea, rather split the functions into groups and infer, like the gleam compiler does
 - Definitions