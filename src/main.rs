use tsuki_backend_llvm::LlvmBackend;
use tsuki_frontend::common::SourceFile;
use tsuki_frontend::backend::Backend;

fn main() {
   LlvmBackend::compile(SourceFile {
      filename: "test.tsu".into(),
      source: r#"
         2 + 2
      "#.into(),
   });
}
