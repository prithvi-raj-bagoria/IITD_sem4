# PowerShell script to compile and run the lexer

# Clean previous build artifacts if they exist
if (Test-Path "lextest.exe") { Remove-Item "lextest.exe" }
if (Test-Path "lexer.ml") { Remove-Item "lexer.ml" }

# Generate lexer.ml from lexer.mll
ocamllex lexer.mll

# Compile all files
ocamlc -o lextest.exe tokens.mli lexer.ml main.ml

if (Test-Path "lextest.exe") {
    # Run with input file if provided
    if ($args.Count -gt 0) {
        ./lextest.exe $args[0]
    }
    else {
        ./lextest.exe input.txt
    }
}
else {
    Write-Host "Compilation failed. Please check the errors above."
}
