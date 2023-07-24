use std::{collections::HashMap, fmt};

use smol_str::SmolStr;

use super::Ty;

#[derive(Clone)]
pub struct TyDisplay<'a> {
    ty: &'a Ty,
    uid: u32,
    env: HashMap<u32, SmolStr>,
}

impl<'a> TyDisplay<'a> {
    pub fn new(ty: &'a Ty) -> Self {
        Self {
            ty,
            uid: 0,
            env: HashMap::new(),
        }
    }

    pub fn next_letter(uid: u32) -> SmolStr {
        let alphabet_length = 26;
        let char_offset = 97;
        let mut chars = vec![];
        let mut n;
        let mut rest = uid;

        loop {
            n = rest % alphabet_length;
            rest /= alphabet_length;
            chars.push((n as u8 + char_offset) as char);

            if rest == 0 {
                break;
            }
            rest -= 1
        }

        chars.into_iter().rev().collect()
    }
}

impl fmt::Display for TyDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut env = self.env.clone();
        let mut uid = self.uid;
        match self.ty {
            Ty::Unknown => "?".fmt(f),
            Ty::Int => "Int".fmt(f),
            Ty::Float => "Float".fmt(f),
            Ty::String => "String".fmt(f),
            Ty::Function { params, return_ } => {
                "fn(".fmt(f)?;
                let return_ = TyDisplay {
                    ty: &return_,
                    uid,
                    env,
                };
                write!(f, ") -> {return_}")
            }
            Ty::Generic { idx } => {
                match env.get(idx) {
                    Some(name) => name.fmt(f),
                    None => {
                        let new_letter = TyDisplay::<'_>::next_letter(uid);
                        env.insert(*idx, new_letter.clone());
                        new_letter.fmt(f)
                    },
                }
            }
            Ty::Tuple { fields } => todo!(),
            Ty::Adt() => todo!(),
        }
    }
}
