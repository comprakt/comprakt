///! Finds files belonging to an integration test.
///!
///! There are 3 allowed ways to specify expected results.
///! They are:
///! - via a yaml front matter block at the beginning of a file
///! - via a separate yaml file with an identical name to the test
///!   but the extension "yaml"
///! - via a file with the suffix `.stderr`, `.stdout`, `.exitcode`
///!   (the original way which annoyed several people)
use crate::yaml;
use failure::{Error, Fail, ResultExt};
use serde::de::DeserializeOwned;
use serde_derive::{Deserialize, Serialize};
use std::{
    ffi::OsStr,
    fs::File,
    io::{self, stdin, stdout, Read, Write},
    path::PathBuf,
};

#[derive(Debug, Fail)]
pub enum DataError {
    #[fail(display = "incorrect yaml front matter in test file {:?}", path)]
    InvalidFrontMatter { path: PathBuf },
    #[fail(
        display = "incorrect yaml in reference output file {:?} for test file {:?}",
        yaml, input
    )]
    InvalidYamlFile { input: PathBuf, yaml: PathBuf },
    #[fail(display = "no reference output for test file {:?} found", path)]
    NoReferenceOutput { path: PathBuf },
}

/// Reference output for stderr, stdout or exit code. This expected output may
/// already be available (because it was read from a yaml file) or saved into a
/// a lazy loaded file.
#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct ReferenceData {
    pub stderr: ExpectedData,
    pub stdout: ExpectedData,
    pub exitcode: ExpectedData,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct OptionalReferenceData {
    pub stderr: Option<ExpectedData>,
    pub stdout: Option<ExpectedData>,
    pub exitcode: Option<ExpectedData>,
}

pub trait IntoReferenceData {
    fn into_reference_data(self, base: &PathBuf) -> ReferenceData;
}

pub trait FromReferencesPath<T> {
    fn from_reference_path(base: &PathBuf) -> T;
}

impl FromReferencesPath<OptionalReferenceData> for OptionalReferenceData {
    fn from_reference_path(base: &PathBuf) -> Self {
        Self::all_from_own_file(base)
    }
}

impl FromReferencesPath<ReferenceData> for ReferenceData {
    fn from_reference_path(base: &PathBuf) -> Self {
        Self::all_from_own_file(base)
    }
}

impl IntoReferenceData for ReferenceData {
    fn into_reference_data(self, _base: &PathBuf) -> ReferenceData {
        self
    }
}

impl IntoReferenceData for OptionalReferenceData {
    fn into_reference_data(self, base: &PathBuf) -> ReferenceData {
        ReferenceData {
            stderr: self
                .stderr
                .unwrap_or_else(|| default_reference_stderr(base)),
            stdout: self
                .stdout
                .unwrap_or_else(|| default_reference_stdout(base)),
            exitcode: self
                .exitcode
                .unwrap_or_else(|| default_reference_exitcode(base)),
        }
    }
}

pub fn default_reference_stderr(base: &PathBuf) -> ExpectedData {
    ExpectedData::InFile(add_extension(base, "stderr"))
}

pub fn default_reference_stdout(base: &PathBuf) -> ExpectedData {
    ExpectedData::InFile(add_extension(base, "stdout"))
}

pub fn default_reference_exitcode(base: &PathBuf) -> ExpectedData {
    ExpectedData::InFile(add_extension(base, "exitcode"))
}

impl OptionalReferenceData {
    pub fn all_from_own_file(base: &PathBuf) -> Self {
        Self {
            stderr: Some(default_reference_stderr(base)),
            stdout: Some(default_reference_stdout(base)),
            exitcode: Some(default_reference_exitcode(base)),
        }
    }
}

impl ReferenceData {
    pub fn all_from_own_file(base: &PathBuf) -> Self {
        Self {
            stderr: default_reference_stderr(base),
            stdout: default_reference_stdout(base),
            exitcode: default_reference_exitcode(base),
        }
    }
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum ExpectedData {
    #[serde(rename = "file")]
    InFile(PathBuf),
    #[serde(rename = "is")]
    Inline(String),
    #[serde(rename = "ignore")]
    Ignore,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub enum InputData {
    WasStripped(String),
    NotStripped(PathBuf, String),
    NotLoaded(PathBuf),
}

/// data given to a test function
#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct TestData<T> {
    pub input: InputData,
    pub reference: T,
}

pub fn get_files<T: FromReferencesPath<T> + DeserializeOwned>(
    path_input: &PathBuf,
    path_references: &PathBuf,
) -> Result<TestData<T>, Error> {
    let input: String = if let Ok(utf8_data) = read(&Some(path_input.clone())) {
        utf8_data
    } else {
        // we do not support yaml in front of non UTF-8 files.
        // Use default
        return Ok(TestData {
            input: InputData::NotLoaded(path_input.clone()),
            reference: T::from_reference_path(path_references),
        });
    };
    let input_file = yaml::FrontMatter::new(&input);

    // 1.) Try to find reference output in yaml front matter
    if let Some(yaml) = input_file.front_matter() {
        let reference: T = serde_yaml::from_str(yaml).context(DataError::InvalidFrontMatter {
            path: path_input.clone(),
        })?;
        return Ok(TestData {
            input: InputData::WasStripped(input_file.without_front_matter().to_string()),
            reference,
        });
    }

    // 2.) Try to find reference output in a separate yaml file
    let path_yaml = path_references.with_extension(".yml");

    if let Ok(yaml) = read(&Some(path_yaml.clone())) {
        let reference: T = serde_yaml::from_str(&yaml).context(DataError::InvalidYamlFile {
            input: path_input.clone(),
            yaml: path_yaml.clone(),
        })?;
        return Ok(TestData {
            input: InputData::NotStripped(
                path_input.clone(),
                input_file.without_front_matter().into(),
            ),
            reference,
        });
    }

    // 3.) Set some default, e.g. expect the data to be available as separate files
    Ok(TestData {
        input: InputData::NotStripped(
            path_input.clone(),
            input_file.without_front_matter().to_string(),
        ),
        reference: T::from_reference_path(path_references),
    })
}

pub fn write(file: &Option<PathBuf>, contents: &str) -> Result<(), Error> {
    if let Some(path) = file {
        let mut file = File::create(path)?;
        file.write_all(contents.as_bytes())?;
    } else {
        stdout().write_all(contents.as_bytes())?;
    };

    Ok(())
}

pub fn read(file: &Option<PathBuf>) -> Result<String, io::Error> {
    // TODO: stream instead of reading everything into string
    let mut contents = String::new();

    if let Some(path) = file {
        File::open(&path)?.read_to_string(&mut contents)?;
    } else {
        stdin().read_to_string(&mut contents)?;
    };

    Ok(contents)
}

/// append another extension to a filename
pub fn add_extension(path: &PathBuf, extension: &str) -> PathBuf {
    let mut filepath = path.clone();

    let original_extension = filepath
        .extension()
        .unwrap_or_else(|| OsStr::new(""))
        .to_os_string();

    filepath.set_extension({
        let mut ext = original_extension.clone();
        ext.push(OsStr::new("."));
        ext.push(OsStr::new(extension));
        ext
    });

    filepath
}
