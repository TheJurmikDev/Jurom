use inkwell::context::Context;
use crate::{Statement};
use super::LLVMCompiler;
use std::process::Command;

pub struct CompilerWrapper;

impl CompilerWrapper {
    pub fn compile_to_exe(statements: Vec<Statement>, output_name: &str) -> Result<(), String> {
        let context = Context::create();
        let mut compiler = LLVMCompiler::new(&context, "main_module")?;

        compiler.compile(statements)?;

        let obj_file = format!("{}.obj", output_name);
        compiler.write_object_file(&obj_file)?;

        Self::link_executable(&obj_file, output_name)?;

        Ok(())
    }

    #[cfg(windows)]
    fn link_executable(obj_file: &str, exe_name_with_ext: &str) -> Result<(), String> {
        Self::try_msvc_linker(obj_file, exe_name_with_ext)
    }

    #[cfg(windows)]
    fn try_msvc_linker(obj_file: &str, exe_name: &str) -> Result<(), String> {
        let output = Command::new("link")
            .args(&[
                obj_file,
                &format!("/OUT:{}", exe_name),
                "/SUBSYSTEM:CONSOLE",
                "/NOLOGO",
                "/MACHINE:X64",
                "/LIBPATH:C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Tools\\MSVC\\14.44.35207\\lib\\x64",
                "/LIBPATH:C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.26100.0\\ucrt\\x64",
                "/LIBPATH:C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.26100.0\\um\\x64",
                "vcruntime.lib",
                "ucrt.lib",
                "msvcrt.lib",
                "legacy_stdio_definitions.lib",
                "kernel32.lib",
                "user32.lib",
            ])
            .output()
            .map_err(|e| format!("Nelze spustit link.exe: {}", e))?;

        if output.status.success() {
            Ok(())
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr);
            let stdout = String::from_utf8_lossy(&output.stdout);
            Err(format!(
                "❌ MSVC Linker failed:\nstdout:\n{}\nstderr:\n{}",
                stdout, stderr
            ))
        }
    }
}