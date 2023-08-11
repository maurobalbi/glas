use std::ops::Index;

use crate::{base::Target, ty, Diagnostic, DiagnosticKind};

use super::{
    module::{AdtData, ConstructorField, FunctionData, ImportData, Param, VariantData, Visibility},
    AstPtr, DefDatabase,
};
use la_arena::{Arena, Idx, IdxRange, RawIdx};
use smol_str::SmolStr;
use syntax::{
    ast::{self, AstNode, Name},
    Parse,
};

#[derive(Default, Debug, Eq, PartialEq)]
pub struct ModuleItemData {
    functions: Arena<FunctionData>,
    imports: Arena<ImportData>,
    adts: Arena<AdtData>,

    variants: Arena<VariantData>,
    pub diagnostics: Vec<Diagnostic>,
}

impl ModuleItemData {
    pub fn imports(
        &self,
    ) -> impl Iterator<Item = (Idx<ImportData>, &ImportData)> + ExactSizeIterator + '_ {
        self.imports.iter()
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

    fn alloc_import(&mut self, import: ImportData) -> Idx<ImportData> {
        self.module_items.imports.alloc(import)
    }

    fn diagnostic(&mut self, diag: Diagnostic) {
        self.module_items.diagnostics.push(diag);
    }

    fn lower_module(&mut self, module: ast::SourceFile) {
        for tg in module.statements() {
            self.lower_target_group(&tg)
        }
    }

    fn lower_target_group(&mut self, tg: &ast::TargetGroup) {
        let package_info = self.db.source_root_package_info(crate::SourceRootId(0));
        if let (Some(token), Some(package_info)) =
            (tg.target().and_then(|t| t.token()), package_info)
        {
            if Target::from(token.text()) != package_info.target {
                self.diagnostic(Diagnostic::new(
                    tg.syntax().text_range(),
                    DiagnosticKind::InactiveTarget,
                ));
                return;
            }
        }
        for statement in tg.statements() {
            self.lower_module_statement(&statement)
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

        let module_name: SmolStr = i
            .module_path()
            .into_iter()
            .filter_map(|t| Some(format!("{}", t.token()?.text())))
            .collect();

        for unqualified in i.unqualified() {
            if let Some(unqualified_name) = unqualified.name().and_then(|t| t.text()) {
                let unqualified_as_name: Option<SmolStr> =
                    unqualified.as_name().and_then(|t| t.text());

                self.alloc_import(ImportData {
                    module: module_name.clone(),
                    unqualified_as_name,
                    unqualified_name,
                    ast_ptr: ast_ptr.clone(),
                });
            }
        }
    }

    fn lower_function(&mut self, fun: &ast::Function) -> Option<Idx<FunctionData>> {
        let ast_ptr = AstPtr::new(fun);

        let mut params = Vec::new();

        if let Some(param_list) = fun.param_list() {
            for param in param_list.params() {
                if let Some(param_name) = param.pattern()?.text() {
                    params.push(Param {
                        name: param_name.into(),
                        label: param.label().and_then(|n| n.text()),
                    });
                }
            }
        };

        Some(self.alloc_function(FunctionData {
            name: fun.name()?.text()?,
            params,
            visibility: Visibility::Public,
            ast_ptr: ast_ptr,
        }))
    }

    fn lower_custom_type(&mut self, ct: &ast::Adt) -> Option<Idx<AdtData>> {
        let ast_ptr = AstPtr::new(ct);
        let name = ct.name()?.text()?;

        let visibility = Visibility::Public;

        let constructors = self.lower_constructors(ct.constructors());
        Some(self.alloc_custom_type(AdtData {
            name,
            variants: constructors,
            params: Vec::new(),
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
                if let Some(type_ref) = self.ty_from_ast_opt(field.type_()) {
                    fields_vec.push(ConstructorField {
                        label: field.label().and_then(|t| t.text()),
                        type_ref,
                    })
                }
            }
        }

        Some(self.alloc_variant(VariantData {
            name,
            fields: fields_vec,
            ast_ptr,
        }))
    }

    fn ty_from_ast_opt(&self, type_ast: Option<ast::TypeExpr>) -> Option<ty::Ty> {
        match type_ast {
            Some(t) => Some(self.ty_from_ast(t)),
            None => None,
        }
    }

    fn ty_from_ast(&self, ast_expr: ast::TypeExpr) -> ty::Ty {
        tracing::info!("{:?}", ast_expr);
        match ast_expr {
            ast::TypeExpr::FnType(_fn_type) => todo!(),
            ast::TypeExpr::TupleType(_) => todo!(),
            ast::TypeExpr::TypeNameRef(t) => {
                if let Some(ty) = t.constructor_name().and_then(|t| t.text()) {
                    match ty.as_str() {
                        "Int" => return ty::Ty::Int,
                        "Float" => return ty::Ty::Float,
                        "String" => return ty::Ty::String,
                        a => {
                            return ty::Ty::Unknown
                        }
                    }
                }
                ty::Ty::Unknown
            }
            ast::TypeExpr::TypeApplication(_) => todo!(),
        }
    }

    fn next_constructor_idx(&self) -> Idx<VariantData> {
        Idx::from_raw(RawIdx::from(self.module_items.variants.len() as u32))
    }
}
