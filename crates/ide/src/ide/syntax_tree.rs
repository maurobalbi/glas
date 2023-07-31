use syntax::{NodeOrToken, TextRange};

use crate::{DefDatabase, FileId};

use super::RootDatabase;

pub(crate) fn syntax_tree(db: &RootDatabase, file_id: FileId) -> String {
    let parse = db.parse(file_id);
    format!("{:#?}", parse.syntax_node())
}
