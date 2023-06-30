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


TypeName