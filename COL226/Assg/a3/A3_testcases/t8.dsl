//import Matrix

// Matrix inversion with singularity check

// Load matrix
matrix A := input(A3_testcases/t8_A.txt);

// Check if invertible
float determinant := det(A);
if (determinant != 0.0) {
    // Calculate inverse
    matrix A_inv := inverse(A);
    print(A_inv);
}
else {
    print(9999999);
}
