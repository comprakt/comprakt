pub struct FrontMatter<'a> {
    yaml: Option<&'a str>,
    contents: &'a str,
}

impl<'a> FrontMatter<'a> {
    pub fn new(file: &'a str) -> Self {
        Self::split(file)
    }

    pub fn has_front_matter(&self) -> bool {
        self.yaml.is_some()
    }

    pub fn front_matter(&'a self) -> Option<&'a str> {
        self.yaml
    }

    pub fn without_front_matter(&self) -> &'a str {
        self.contents
    }

    fn split(contents: &'a str) -> Self {
        let mut parts = contents.trim_start().splitn(2, "\n---\n");

        match (parts.next(), parts.next()) {
            (Some(yaml), Some(mini_java)) if yaml.starts_with("---\n") => Self {
                yaml: Some(yaml),
                contents: mini_java,
            },
            _ => Self {
                yaml: None,
                contents,
            },
        }
    }
}
