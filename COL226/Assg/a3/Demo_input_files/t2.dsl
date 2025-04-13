// Linear algebra operations using matrix operations

// Load input data
matrix A := input(Demo_input_files/t2_A.txt);
matrix B := input(Demo_input_files/t2_B.txt);
matrix D := input(Demo_input_files/t2_D.txt);
vector u := input(Demo_input_files/t2_u.txt);

// Matrix addition
matrix C := A + B;

// Matrix multiplication
matrix E := C * D;

// Check if invertible and solve linear system
float det_val := det(E);
if (det_val != 0.0) {
    // Compute inverse and solve for x
    matrix E_inv := inverse(E);
    vector x := E_inv * u;
    print(x);
}
else {
    print(9999999);  // Error code for non-invertible matrix
}