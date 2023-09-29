// use itertools::Itertools;
use smol_str::SmolStr;
use std::collections::{HashMap, HashSet};
use syntax::Error;

pub trait HasLabel {
    fn label(self) -> Option<SmolStr>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldMap {
    pub arity: u32,
    pub fields: HashMap<SmolStr, u32>,
}

#[derive(Debug, Clone, Copy)]
pub struct DuplicateField;

impl FieldMap {
    pub fn new(arity: u32) -> Self {
        Self {
            arity,
            fields: HashMap::new(),
        }
    }

    pub fn insert(&mut self, label: SmolStr, index: u32) -> Result<(), DuplicateField> {
        match self.fields.insert(label, index) {
            Some(_) => Err(DuplicateField),
            None => Ok(()),
        }
    }

    pub fn into_option(self) -> Option<Self> {
        if self.fields.is_empty() {
            None
        } else {
            Some(self)
        }
    }

    // /// Reorder an argument list so that labelled fields supplied out-of-order are
    // /// in the correct order.
    // ///
    // pub fn reorder(&self, args: &mut [impl HasLabel]) -> Result<(), Error> {
    //     let mut labelled_arguments_given = false;
    //     let mut seen_labels = std::collections::HashSet::new();
    //     let mut unknown_labels = Vec::new();

    //     if self.arity as usize != args.len() {
    //         return Err(Error::IncorrectArity {
    //             labels: self.incorrect_arity_labels(args),
    //             location,
    //             expected: self.arity as usize,
    //             given: args.len(),
    //         });
    //     }

    //     for arg in args.iter() {
    //         match &arg.label {
    //             Some(_) => {
    //                 labelled_arguments_given = true;
    //             }

    //             None => {
    //                 if labelled_arguments_given && !arg.implicit {
    //                     return Err(Error::PositionalArgumentAfterLabelled {
    //                         location: arg.location,
    //                     });
    //                 }
    //             }
    //         }
    //     }

    //     let mut i = 0;
    //     while i < args.len() {
    //         let (label, &location) = match &args.get(i).expect("Field indexing to get label").label
    //         {
    //             // A labelled argument, we may need to reposition it in the array vector
    //             Some(l) => (
    //                 l,
    //                 &args
    //                     .get(i)
    //                     .expect("Indexing in labelled field reordering")
    //                     .location,
    //             ),

    //             // Not a labelled argument
    //             None => {
    //                 i += 1;
    //                 continue;
    //             }
    //         };

    //         let position = match self.fields.get(label) {
    //             None => {
    //                 unknown_labels.push((label.clone(), location));
    //                 i += 1;
    //                 continue;
    //             }

    //             Some(&p) => p,
    //         };

    //         // If the argument is already in the right place
    //         if position as usize == i {
    //             let _ = seen_labels.insert(label.clone());
    //             i += 1;
    //         } else {
    //             if seen_labels.contains(label) {
    //                 return Err(Error::DuplicateArgument {
    //                     location,
    //                     label: label.clone(),
    //                 });
    //             }
    //             let _ = seen_labels.insert(label.clone());

    //             args.swap(position as usize, i);
    //         }
    //     }

    //     if unknown_labels.is_empty() {
    //         Ok(())
    //     } else {
    //         // Err(Error::UnknownLabels {
    //         //     valid: self.fields.keys().cloned().collect(),
    //         //     unknown: unknown_labels,
    //         //     supplied: seen_labels.into_iter().collect(),
    //         // })
    //         Ok(())
    //     }
    // }

    // pub fn incorrect_arity_labels<A>(&self, args: &[impl HasLabel]) -> Vec<SmolStr> {
    //     let given: HashSet<_> = args.iter().filter_map(|arg| arg.label().as_ref()).collect();

    //     self.fields
    //         .keys()
    //         .cloned()
    //         .filter(|f| !given.contains(f))
    //         .sorted()
    //         .collect()
    // }
}

#[derive(Debug)]
pub struct FieldMapBuilder {
    index: u32,
    any_labels: bool,
    field_map: FieldMap,
}

impl FieldMapBuilder {
    pub fn new(size: u32) -> Self {
        Self {
            index: 0,
            any_labels: false,
            field_map: FieldMap::new(size),
        }
    }

    pub fn add(&mut self, label: Option<&SmolStr>) -> Result<(), Error> {
        match label {
            Some(label) => self.labelled(label)?,
            None => self.unlabelled()?,
        }
        self.index += 1;
        Ok(())
    }

    fn labelled(&mut self, label: &SmolStr) -> Result<(), Error> {
        if self.field_map.insert(label.clone(), self.index).is_err() {
            // return Err(Error::DuplicateField {
            //     label: label.clone(),
            //     location,
            // });
        };
        self.any_labels = true;
        Ok(())
    }

    fn unlabelled(&mut self) -> Result<(), Error> {
        if self.any_labels {
            // return Err(Error::UnlabelledAfterlabelled { location });
        }
        Ok(())
    }

    pub fn finish(self) -> Option<FieldMap> {
        self.field_map.into_option()
    }
}