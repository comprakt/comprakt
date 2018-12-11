use crate::*;
use serde_derive::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Deserialize, Serialize, Clone, Default)]
pub struct Data {
    pub stderr: Option<ExpectedData>,
    pub stdout: Option<ExpectedData>,
    pub exitcode: Option<ExpectedData>,
    pub content: Option<String>,
    pub kind: Option<String>,
    pub typeinfo: Option<String>,
}

impl FromReferencesPath<Data> for Data {
    fn from_reference_path(_base: &PathBuf) -> Self {
        Self::default()
    }
}

impl IntoReferenceData for Data {
    fn into_reference_data(self, base: &PathBuf) -> ReferenceData {
        self.into_reference_data(base)
    }
}

impl Data {
    fn into_reference_data(self, base: &PathBuf) -> ReferenceData {
        ReferenceData {
            stderr: self
                .stderr
                .unwrap_or_else(|| ExpectedData::Inline("".to_owned())),
            stdout: self
                .stdout
                .unwrap_or_else(|| default_reference_stdout(&base)),
            exitcode: self
                .exitcode
                .unwrap_or_else(|| ExpectedData::Inline("0".to_owned())),
        }
    }
}

pub fn exec_ast_inspector_test(input: PathBuf) {
    let spec = TestSpec {
        references: input.clone(),
        input,
        generate_tentatives: true,
    };

    let (input_without_yaml_path, data) = load_test_data::<Data>(&spec);

    let callinfo = CompilerCall::AstInspector {
        content: data.reference.content.clone(),
        kind: data.reference.kind.clone(),
        typeinfo: data.reference.typeinfo.clone(),
    };

    let mut call = compiler_call(callinfo, &input_without_yaml_path);
    println!("Executing: {:?}", call);

    let output = call.output().expect("failed to call ast-inspector");

    assert_output(
        &output,
        data.reference.clone().into_reference_data(&spec.references),
        &spec,
    );
}
