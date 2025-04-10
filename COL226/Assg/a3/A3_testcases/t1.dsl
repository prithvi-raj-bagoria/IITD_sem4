// Linear regression solver using normal equations

// Load input data
matrix A := input(A3_testcases/t1_A.txt);
vector b := input(A3_testcases/t1_b.txt);

// Compute A^T * A
matrix A_T := trans(A);
matrix A_TA := A_T * A;

// Check if invertible
float det_val := det(A_TA);
if (det_val != 0.0) {
    // Solve normal equations: theta = (A^T*A)^(-1) * A^T * b
    matrix A_TA_inv := inverse(A_TA);
    vector A_Tb := A_T * b;
    vector theta := A_TA_inv * A_Tb;
    print(theta);
}
else {
    print(9999999);
}