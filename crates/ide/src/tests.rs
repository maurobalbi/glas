use crate::base::SourceDatabaseStorage;
use crate::def::{DefDatabaseStorage, InternDatabaseStorage};
use crate::ide::Upcast;
use crate::ty::TyDatabaseStorage;
use crate::{
    Change, DefDatabase, FileId, FilePos, FileRange, FileSet, PackageGraph, PackageInfo,
    SourceRoot, VfsPath,
};
use anyhow::{bail, ensure, Context, Result};
use indexmap::IndexMap;
use smol_str::SmolStr;

use std::{mem, ops};
use syntax::{TextRange, TextSize};

pub const MARKER_INDICATOR: char = '$';

#[salsa::database(
    InternDatabaseStorage,
    SourceDatabaseStorage,
    DefDatabaseStorage,
    TyDatabaseStorage
)]
#[derive(Default)]
pub struct TestDB {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for TestDB {}

impl Upcast<dyn DefDatabase> for TestDB {
    fn upcast(&self) -> &(dyn DefDatabase + 'static) {
        self
    }
}

impl TestDB {
    pub fn single_file(fixture: &str) -> Result<(Self, FileId)> {
        let (db, f) = Self::from_fixture(fixture)?;
        // gleam.toml is always added aswell
        ensure!(f.files().len() == 2, "Fixture contains multiple files");
        let file_id = f.files()[0];
        Ok((db, file_id))
    }

    pub fn from_fixture(fixture: &str) -> Result<(Self, Fixture)> {
        let f = Fixture::new(fixture)?;
        let mut db = Self::default();
        let mut change = Change::default();
        let mut file_set = FileSet::default();
        for (i, (path, text)) in (0u32..).zip(&f.files) {
            let file = FileId(i);
            file_set.insert(file, path.clone());

            change.change_file(file, text.to_owned().into());
        }
        change.set_roots(vec![SourceRoot::new(file_set, "/".into())]);
        let mut package_graph = PackageGraph::default();
        package_graph.add_package(
            SmolStr::from("test"),
            FileId(f.files.len() as u32 - 1),
            true,
        );

        change.set_package_graph(package_graph);
        change.apply(&mut db);
        Ok((db, f))
    }
}

#[derive(Default, Debug)]
pub struct Fixture {
    files: IndexMap<VfsPath, String>,
    file_ids: Vec<FileId>,
    markers: Vec<FilePos>,
    package_info: Option<PackageInfo>,
}

impl ops::Index<usize> for Fixture {
    type Output = FilePos;
    fn index(&self, index: usize) -> &Self::Output {
        &self.markers[index]
    }
}

impl<'a> ops::Index<&'a str> for Fixture {
    type Output = FileId;
    fn index(&self, index: &'a str) -> &Self::Output {
        let id = self.files.get_index_of(&VfsPath::new(index)).unwrap();
        &self.file_ids[id]
    }
}

impl Fixture {
    pub fn new(fixture: &str) -> Result<Self> {
        ensure!(
            u32::try_from(fixture.len()).is_ok(),
            "Size too large: {}",
            fixture.len()
        );

        let mut this = Self::default();
        let mut missing_header = false;
        let mut cur_path = None;
        let mut cur_text = String::new();
        let mut cur_file = FileId(0);
        let mut markers = [None; 10];
        for line in fixture.lines().skip_while(|line| line.is_empty()) {
            if let Some(header) = line.strip_prefix("#- ") {
                ensure!(!missing_header, "Missing path header at the first line");

                let mut iter = header.split(' ');
                let test_dir = VfsPath::new("/test");
                let path = iter.next().context("Missing path")?;
                let path = test_dir.join(path).unwrap();

                for prop in iter {
                    if let Some((_name, target)) = prop
                        .strip_prefix("input:")
                        .and_then(|input| input.split_once('='))
                    {
                        let _target = VfsPath::new(target);
                    } else {
                        bail!("Unknow property {prop}");
                    }
                }

                if let Some(prev_path) = cur_path.replace(path) {
                    this.insert_file(prev_path, mem::take(&mut cur_text))?;
                    cur_file.0 += 1;
                }
            } else {
                if cur_path.is_none() {
                    missing_header = true;
                    cur_path = Some(VfsPath::new("/test/test.gleam".to_string()));
                }

                let mut iter = line.chars().peekable();
                while let Some(ch) = iter.next() {
                    if ch == MARKER_INDICATOR
                        && matches!(iter.peek(), Some(c) if c.is_ascii_digit())
                    {
                        let n = iter.next().unwrap().to_digit(10).unwrap() as usize;
                        let pos =
                            FilePos::new(cur_file, TextSize::try_from(cur_text.len()).unwrap());
                        ensure!(
                            markers[n].replace(pos).is_none(),
                            "Duplicated marker: {}",
                            n
                        );
                    } else {
                        cur_text.push(ch);
                    }
                }
                cur_text += "\n";
            }
        }
        this.insert_file(cur_path.context("Empty fixture")?, cur_text)?;

        let _: std::result::Result<(), anyhow::Error> =
            this.insert_file(VfsPath::new("/gleam.toml"), "".to_string());
        this.package_info.get_or_insert_with(|| PackageInfo {
            gleam_toml: FileId(this.files.len() as u32 - 1),
            dependencies: Default::default(),
            display_name: "Test".into(),
            is_local: true,
        });

        let marker_len = markers
            .iter()
            .rposition(|p| p.is_some())
            .map_or(0, |n| n + 1);
        this.markers = markers
            .into_iter()
            .take(marker_len)
            .enumerate()
            .map(|(i, p)| p.with_context(|| format!("Discontinuous marker: {}", i)))
            .collect::<Result<Vec<_>>>()?;

        tracing::info!("{:?}", this);
        Ok(this)
    }

    fn insert_file(&mut self, path: VfsPath, mut text: String) -> Result<()> {
        let file = FileId(self.files.len() as u32);
        text.truncate(text.trim_end().len());
        ensure!(
            self.files.insert(path.clone(), text).is_none(),
            "Duplicated path: {:?}",
            path
        );
        self.file_ids.push(file);
        Ok(())
    }

    pub fn files(&self) -> &[FileId] {
        &self.file_ids
    }

    pub fn markers(&self) -> &[FilePos] {
        &self.markers
    }

    #[track_caller]
    pub fn unwrap_single_range_marker(&self) -> FileRange {
        match *self.markers() {
            [fpos] => FileRange::empty(fpos),
            [start, end] => {
                assert_eq!(
                    start.file_id, end.file_id,
                    "Start and end markers must be in the same file"
                );
                FileRange::new(start.file_id, TextRange::new(start.pos, end.pos))
            }
            _ => panic!("Must have either 1 or 2 markers"),
        }
    }
}
