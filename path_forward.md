1. lower whole module, populate arenas and dyn_map with ast -> arena idx and reverse
2. collect scopes with exprscope and modulescope and genericscope (could be difficult because function needs to be infered first)
3. infer types
4. for most names its probably enough to use #1 and #2 for resolution, however for field_access its necessary to infer type of field_access expr first.


idea: need to intern params, to be able to map them from param to type!