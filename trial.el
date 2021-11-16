(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(use-package dap-mode
  :ensure
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
	 :gdbpath "rust-lldb"
         :target nil
         :cwd nil)))

(setq dap-cpptools-extension-version "1.5.1")

(with-eval-after-load 'lsp-rust
  (require 'dap-cpptools))

(with-eval-after-load 'dap-mode
  (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
  (dap-auto-configure-mode +1))

(with-eval-after-load 'dap-cpptools
  ;; Add a template specific for debugging Rust programs.
  ;; It is used for new projects, where I can M-x dap-edit-debug-template
  (dap-register-debug-template
   "Rust::CppTools Run Configuration"
   (list :type "cppdbg"
         :request "launch"
         :name "Rust::Run"
         :MIMode "gdb"
         :miDebuggerPath "rust-gdb"
         :environment []
         :program "${workspaceFolder}/target/debug/stolif"
         :cwd "${workspaceFolder}"
         :console "external"
         :dap-compilation "cargo build"
         :dap-compilation-dir "${workspaceFolder}")))
