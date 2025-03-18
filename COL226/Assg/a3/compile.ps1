# PowerShell script to compile the OCaml parser project

# Set error action preference
$ErrorActionPreference = "Stop"

# Output information
Write-Host "Compiling OCaml parser project..."

try {
    # Generate parser from mly file
    Write-Host "Generating parser from parser.mly..."
    ocamlyacc parser.mly
    if (-not $?) { throw "ocamlyacc failed" }

    # Generate lexer from mll file
    Write-Host "Generating lexer from lexer.mll..."
    ocamllex lexer.mll
    if (-not $?) { throw "ocamllex failed" }

    # Compile modules in order of dependency
    Write-Host "Compiling AST..."
    ocamlc -c ast.ml
    if (-not $?) { throw "Failed to compile ast.ml" }

    Write-Host "Compiling parser..."
    ocamlc -c parser.mli
    ocamlc -c parser.ml
    if (-not $?) { throw "Failed to compile parser.ml" }

    Write-Host "Compiling lexer..."
    ocamlc -c lexer.ml
    if (-not $?) { throw "Failed to compile lexer.ml" }

    Write-Host "Compiling typechecker..."
    ocamlc -c typechecker.ml
    if (-not $?) { throw "Failed to compile typechecker.ml" }

    Write-Host "Compiling main driver..."
    ocamlc -c main.ml
    if (-not $?) { throw "Failed to compile main.ml" }

    # Link to create the main executable
    Write-Host "Linking to create main.exe..."
    ocamlc -o main.exe ast.cmo parser.cmo lexer.cmo typechecker.cmo main.cmo
    if (-not $?) { throw "Failed to link main.exe" }

    Write-Host "Compilation successful! Run your project with: .\main.exe <input_file>"
} 
catch {
    Write-Host "Error: $_" -ForegroundColor Red
    exit 1
}

# Function to clean up compiled files
function Clean-Build {
    Write-Host "Cleaning build files..."
    Remove-Item -ErrorAction SilentlyContinue *.cmo, *.cmi, parser.ml, parser.mli, lexer.ml, main.exe
    Write-Host "Clean complete."
}

# Check for clean parameter
if ($args.Contains("clean")) {
    Clean-Build
}
