#[derive(Debug, Clone)]
pub struct Config {
    pub name: String,
    pub version: String,
    pub author: String,
    pub mode: String,
    pub transpiler_mode: String,
    pub src_path: String,
    pub main_file: String,
}