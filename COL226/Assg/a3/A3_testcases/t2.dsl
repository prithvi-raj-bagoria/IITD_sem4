//import Matrix

// Matrix operations with dimension checking

// Load matrices and vector
matrix A := input(A3_testcases/t2_A.txt);
matrix B := input(A3_testcases/t2_B.txt);
matrix D := input(A3_testcases/t2_D.txt);
vector u := input(A3_testcases/t2_u.txt);

// Matrix addition
matrix C := A + B;

// Matrix multiplication
matrix E := C * D;

// Check if invertible and solve system
if (det(E) != 0.0) {
    matrix E_inverse := inverse(E);
    vector x := E_inverse * u;
    print(x);
}
else {
    print(9999999);
}