# compile.ps1 - Compilation script for Matrix/Vector DSL
param (
    [switch]$clean = $false,
    [switch]$run = $false,
    [switch]$verbose = $false
)

# Cleaning function
function Clean-Project {
    echo "Cleaning project files..."
    # Remove generated OCaml files
    if (Test-Path lexer.ml) { Remove-Item lexer.ml }
    if (Test-Path parser.ml) { Remove-Item parser.ml }
    if (Test-Path parser.mli) { Remove-Item parser.mli }
    if (Test-Path parser.output) { Remove-Item parser.output }
    
    # Remove object files
    Get-ChildItem -Filter "*.cmo" | Remove-Item
    Get-ChildItem -Filter "*.cmi" | Remove-Item
    
    # Remove executable
    if (Test-Path dsl.exe) { Remove-Item dsl.exe }
    
    echo "Clean complete."
    exit 0
}

# Check if clean option was specified
if ($clean) {
    Clean-Project
}

# Check for OCaml tools
echo "Checking for OCaml tools..."
if (-not (Get-Command ocamllex -ErrorAction SilentlyContinue)) {
    Write-Error "ocamllex not found. Please ensure OCaml is installed properly."
    exit 1
}
if (-not (Get-Command ocamlyacc -ErrorAction SilentlyContinue)) {
    Write-Error "ocamlyacc not found. Please ensure OCaml is installed properly."
    exit 1
}

# Step 1: Generate lexer and parser
echo "Generating lexer..."
ocamllex lexer.mll
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to process lexer.mll"
    exit 1
}

echo "Generating parser..."
if ($verbose) {
    # Generate verbose output for debugging conflicts
    ocamlyacc -v parser.mly
} else {
    ocamlyacc parser.mly
}
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to process parser.mly"
    exit 1
}

# Step 2: Compile modules in correct dependency order
echo "Compiling modules..."

# First, compile the AST definition
echo "Compiling ast.ml..."
ocamlc -c ast.ml
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to compile ast.ml"
    exit 1
}

# Compile the tokens interface
echo "Compiling tokens.mli..."
ocamlc -c tokens.mli
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to compile tokens.mli"
    exit 1
}

# Important: compile parser.mli BEFORE parser.ml
echo "Compiling parser.mli..."
ocamlc -c parser.mli
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to compile parser.mli"
    exit 1
}

# Now compile implementation files
echo "Compiling parser.ml..."
ocamlc -c parser.ml
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to compile parser.ml"
    exit 1
}

echo "Compiling lexer.ml..."
ocamlc -c lexer.ml
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to compile lexer.ml"
    exit 1
}

echo "Compiling typechecker.ml..."
ocamlc -c typechecker.ml
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to compile typechecker.ml"
    exit 1
}

echo "Compiling main.ml..."
ocamlc -c main.ml
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to compile main.ml"
    exit 1
}

# Step 3: Link everything together (excluding tokens.cmo since it's only an interface)
echo "Linking final executable..."
ocamlc -o dsl.exe ast.cmo parser.cmo lexer.cmo typechecker.cmo main.cmo
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to link modules into executable"
    exit 1
}

# Step 4: Run with input.txt if requested or -run flag is set
if ($run -or (-not $clean)) {
    echo "Running with input from input.txt..."
    if (-not (Test-Path "input.txt")) {
        Write-Error "input.txt not found in the current directory."
        exit 1
    }
    
    echo "AST Output:"
    Get-Content "input.txt" | .\dsl.exe
}

echo "Done."
