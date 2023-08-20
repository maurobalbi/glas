use syntax::ast;

use crate::{DefDatabase, InFile};

use super::hir::{Function, Adt, Variant};


pub trait HasSource {
    type Ast;
    /// Fetches the definition's source node.
    /// Using [`crate::Semantics::source`] is preferred when working with [`crate::Semantics`],
    /// as that caches the parsed file in the semantics' cache.
    fn source(self, db: &dyn DefDatabase) -> Option<InFile<Self::Ast>>;
}

impl HasSource for Function {
    type Ast = ast::Function;
    fn source(self, db: &dyn DefDatabase) -> Option<InFile<Self::Ast>> {
        let loc = db.lookup_intern_function(self.id);
        let fn_data = &db.module_items(loc.file_id)[loc.value];
        let root = db.parse(loc.file_id);
        Some(loc.map(|_| fn_data.ast_ptr.to_node(&root.syntax_node())))
    }
}

impl HasSource for Adt {
    type Ast = ast::Adt;
    fn source(self, db: &dyn DefDatabase) -> Option<InFile<Self::Ast>> {
        let loc = db.lookup_intern_adt(self.id);
        let adt_data = &db.module_items(loc.file_id)[loc.value];
        let root = db.parse(loc.file_id);
        Some(loc.map(|_| adt_data.ast_ptr.to_node(&root.syntax_node())))
    }
}

impl HasSource for Variant {
    type Ast = ast::Variant;
    fn source(self, db: &dyn DefDatabase) -> Option<InFile<Self::Ast>> {
        let loc = db.lookup_intern_adt(self.parent);
        let variant_data = &db.module_items(loc.file_id)[self.id];
        let root = db.parse(loc.file_id);
        Some(loc.map(|_| variant_data.ast_ptr.to_node(&root.syntax_node())))
    }
}