# compile.ps1 - PowerShell script to compile Matrix/Vector DSL on Windows
# Author: Prithvi Raj Bagoria
# Created: 2023-03-17

# Function to check if a command was successful and exit if not
function Check-Exit {
    param([string]$step)
    if (-not $?) {
        Write-Host "Error during $step. Exiting." -ForegroundColor Red
        exit 1
    }
}

# Print header
Write-Host "========================================" -ForegroundColor Cyan
Write-Host "  Matrix/Vector DSL Compilation Script  " -ForegroundColor Cyan
Write-Host "========================================" -ForegroundColor Cyan
Write-Host ""

# Check if OCaml is installed and available
try {
    $ocamlVersion = ocamlc -version
    Write-Host "Using OCaml version: $ocamlVersion" -ForegroundColor Green
} catch {
    Write-Host "OCaml not found. Please install OCaml and make sure it's in your PATH." -ForegroundColor Red
    exit 1
}

# Clean up any existing compiled files
Write-Host "Cleaning up previous compilation files..." -ForegroundColor Yellow
Remove-Item -ErrorAction SilentlyContinue *.cmo, *.cmi, parser.ml, parser.mli, lexer.ml, matrix_dsl.exe

# Step 1: Compile AST
Write-Host "Compiling ast.ml..." -ForegroundColor Yellow
ocamlc -c ast.ml
Check-Exit "AST compilation"

# Step 2: Process parser with ocamlyacc
Write-Host "Processing parser.mly with ocamlyacc..." -ForegroundColor Yellow
ocamlyacc parser.mly
Check-Exit "parser generation"

# Step 3: Compile parser interface
Write-Host "Compiling parser.mli..." -ForegroundColor Yellow
ocamlc -c parser.mli
Check-Exit "parser interface compilation"

# Step 4: Process lexer with ocamllex
Write-Host "Processing lexer.mll with ocamllex..." -ForegroundColor Yellow
ocamllex lexer.mll
Check-Exit "lexer generation"

# Step 5: Compile parser implementation
Write-Host "Compiling parser.ml..." -ForegroundColor Yellow
ocamlc -c parser.ml
Check-Exit "parser implementation compilation"

# Step 6: Compile lexer
Write-Host "Compiling lexer.ml..." -ForegroundColor Yellow
ocamlc -c lexer.ml
Check-Exit "lexer compilation"

# Step 7: Compile type checker
Write-Host "Compiling typecheck.ml..." -ForegroundColor Yellow
ocamlc -c typecheck.ml
Check-Exit "type checker compilation"

# Step 8: Compile main module
Write-Host "Compiling main.ml..." -ForegroundColor Yellow
ocamlc -c main.ml
Check-Exit "main module compilation"

# Step 9: Link all modules to create executable
Write-Host "Linking all modules to create executable..." -ForegroundColor Yellow
ocamlc -o matrix_dsl.exe ast.cmo parser.cmo lexer.cmo typecheck.cmo main.cmo
Check-Exit "linking"

# Check if executable was created successfully
if (Test-Path -Path "matrix_dsl.exe") {
    Write-Host ""
    Write-Host "Compilation successful!" -ForegroundColor Green
    Write-Host "Created executable: matrix_dsl.exe" -ForegroundColor Green
    Write-Host ""
    Write-Host "To run the program: .\matrix_dsl.exe <filename>" -ForegroundColor Cyan
    Write-Host ""
} else {
    Write-Host "Failed to create executable." -ForegroundColor Red
    exit 1
}