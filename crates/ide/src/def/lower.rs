use std::ops::Index;

use crate::{ ty, Diagnostic};

use super::{
    module::{
        AdtData, Field, FunctionData, ImportData, ModuleImport, Param, VariantData, Visibility,
    },
    AstPtr, DefDatabase, fields::FieldMap,
};
use la_arena::{Arena, Idx, IdxRange, RawIdx};
use smol_str::SmolStr;
use syntax::{
    ast::{self, Pattern},
    Parse,
};

#[derive(Default, Debug, Eq, PartialEq)]
pub struct ModuleItemData {
    functions: Arena<FunctionData>,
    unqualified_imports: Arena<ImportData>,
    adts: Arena<AdtData>,

    module_imports: Arena<ModuleImport>,

    variants: Arena<VariantData>,
    pub diagnostics: Vec<Diagnostic>,
}

impl ModuleItemData {
    pub fn unqualified_imports(
        &self,
    ) -> impl Iterator<Item = (Idx<ImportData>, &ImportData)> + ExactSizeIterator + '_ {
        self.unqualified_imports.iter()
    }

    pub fn adts(&self) -> impl Iterator<Item = (Idx<AdtData>, &AdtData)> + ExactSizeIterator + '_ {
        self.adts.iter()
    }

    pub fn variants(
        &self,
    ) -> impl Iterator<Item = (Idx<VariantData>, &VariantData)> + ExactSizeIterator + '_ {
        self.variants.iter()
    }

    pub fn functions(
        &self,
    ) -> impl Iterator<Item = (Idx<FunctionData>, &FunctionData)> + ExactSizeIterator + '_ {
        self.functions.iter()
    }

    pub fn module_imports(
        &self,
    ) -> impl Iterator<Item = (Idx<ModuleImport>, &ModuleImport)> + ExactSizeIterator + '_ {
        self.module_imports.iter()
    }
}

impl Index<Idx<AdtData>> for ModuleItemData {
    type Output = AdtData;

    fn index(&self, index: Idx<AdtData>) -> &Self::Output {
        &self.adts[index]
    }
}

impl Index<Idx<VariantData>> for ModuleItemData {
    type Output = VariantData;

    fn index(&self, index: Idx<VariantData>) -> &Self::Output {
        &self.variants[index]
    }
}

impl Index<Idx<FunctionData>> for ModuleItemData {
    type Output = FunctionData;

    fn index(&self, index: Idx<FunctionData>) -> &Self::Output {
        &self.functions[index]
    }
}

impl Index<Idx<ModuleImport>> for ModuleItemData {
    type Output = ModuleImport;

    fn index(&self, index: Idx<ModuleImport>) -> &Self::Output {
        &self.module_imports[index]
    }
}
struct LowerCtx<'a> {
    db: &'a dyn DefDatabase,
    module_items: ModuleItemData,
}

pub(super) fn lower_module(db: &dyn DefDatabase, parse: Parse) -> ModuleItemData {
    let mut ctx = LowerCtx {
        db,
        module_items: ModuleItemData::default(),
    };
    ctx.lower_module(parse.root());
    ctx.module_items
}

impl<'a> LowerCtx<'a> {
    fn alloc_function(&mut self, function: FunctionData) -> Idx<FunctionData> {
        let id = self.module_items.functions.alloc(function);
        id
    }

    fn alloc_custom_type(&mut self, custom_type: AdtData) -> Idx<AdtData> {
        let id = self.module_items.adts.alloc(custom_type);
        id
    }

    fn alloc_variant(&mut self, constructor: VariantData) -> Idx<VariantData> {
        let id = self.module_items.variants.alloc(constructor);
        id
    }

    fn alloc_unqualified_import(&mut self, import: ImportData) -> Idx<ImportData> {
        self.module_items.unqualified_imports.alloc(import)
    }

    fn alloc_module_import(&mut self, import: ModuleImport) -> Idx<ModuleImport> {
        self.module_items.module_imports.alloc(import)
    }

    fn diagnostic(&mut self, diag: Diagnostic) {
        self.module_items.diagnostics.push(diag);
    }

    fn lower_module(&mut self, module: ast::SourceFile) {
        for stmt in module.statements() {
            self.lower_module_statement(&stmt)
        }
    }

    fn lower_module_statement(&mut self, stmnt: &ast::ModuleStatement) {
        match stmnt {
            // ast::ModuleStatement::ModuleConstant(_) => todo!(),
            // ast::ModuleStatement::Import(_) => todo!(),
            ast::ModuleStatement::Function(f) => {
                self.lower_function(f);
            }
            ast::ModuleStatement::Import(i) => {
                self.lower_import(i);
            }
            ast::ModuleStatement::Adt(ct) => {
                self.lower_custom_type(ct);
            }
            _ => return,
        };
    }
    /// Here were resolving the imports and allocating the
    fn lower_import(&mut self, i: &ast::Import) {
        let ast_ptr = AstPtr::new(i);

        //ToDo: Diagnostics wrong imports!
        let module_name: SmolStr = i
            .module_path()
            .into_iter()
            .flat_map(|m| m.path())
            .filter_map(|t| Some(format!("{}", t.token()?.text())))
            .collect::<Vec<_>>()
            .join("/")
            .into();

        let accessor: SmolStr = i
            .module_path()
            .into_iter()
            .flat_map(|m| m.path())
            .filter_map(|t| Some(format!("{}", t.token()?.text())))
            .last()
            .expect("This is a compiler bug")
            .into();

        let module_id = self.alloc_module_import(ModuleImport {
            name: module_name,
            accessor,
            ast_ptr,
        });

        for unqualified in i.unqualified() {
            if let Some(unqualified_name) = unqualified.name().and_then(|t| t.text()) {
                let ast_ptr = AstPtr::new(&unqualified);
                let unqualified_as_name: Option<SmolStr> =
                    unqualified.as_name().and_then(|t| t.text());

                self.alloc_unqualified_import(ImportData {
                    module: module_id.clone(),
                    unqualified_as_name,
                    unqualified_name,
                    ast_ptr: ast_ptr,
                });
            }
        }
    }

    fn lower_function(&mut self, fun: &ast::Function) -> Option<Idx<FunctionData>> {
        let ast_ptr = AstPtr::new(fun);

        
        let mut params = Vec::new();
        if let Some(param_list) = fun.param_list() {
            for param in param_list.params() {
                match param.pattern() {
                    Some(Pattern::PatternVariable(it)) => {
                        it.text().map(|t| {
                            params.push(Param {
                                name: t,
                                label: param.label().and_then(|n| n.text()),
                            })
                        });
                    }
                    _ => {},
                }
            }
        };

        Some(self.alloc_function(FunctionData {
            name: fun.name()?.text()?,
            params,
            param_map: FieldMap::new(0),
            visibility: Visibility::Public,
            ast_ptr: ast_ptr,
        }))
    }

    fn lower_custom_type(&mut self, ct: &ast::Adt) -> Option<Idx<AdtData>> {
        let ast_ptr = AstPtr::new(ct);
        let name = ct.name()?.text()?;
        let mut generic_params = Vec::new();
        if let  Some(params) = ct.generic_params() {
            for param in params.params() {
                generic_params.push(ty::ty_from_ast(param));
            }
        }

        let visibility = Visibility::Public;

        let constructors = self.lower_constructors(ct.constructors());
        Some(self.alloc_custom_type(AdtData {
            name,
            variants: constructors,
            params: generic_params,
            visibility,
            ast_ptr,
        }))
    }

    fn lower_constructors(
        &mut self,
        constructors: ast::AstChildren<ast::Variant>,
    ) -> IdxRange<VariantData> {
        let start = self.next_constructor_idx();
        for constructor in constructors {
            self.lower_constructor(&constructor);
        }
        let end = self.next_constructor_idx();
        IdxRange::new(start..end)
    }

    fn lower_constructor(&mut self, constructor: &ast::Variant) -> Option<Idx<VariantData>> {
        let ast_ptr = AstPtr::new(constructor);
        let name = constructor.name()?.text()?;

        let mut fields_vec = Vec::new();
        if let Some(fields) = constructor.field_list() {
            for field in fields.fields() {
                if let Some(type_ref) = ty::ty_from_ast_opt(field.type_()) {
                    fields_vec.push(Field {
                        label: field.label().and_then(|t| t.text()),
                        type_ref,
                    })
                }
            }
        }

        Some(self.alloc_variant(VariantData {
            name,
            fields: fields_vec,
            field_map: FieldMap::new(0),
            ast_ptr,
        }))
    }

    fn next_constructor_idx(&self) -> Idx<VariantData> {
        Idx::from_raw(RawIdx::from(self.module_items.variants.len() as u32))
    }
}
