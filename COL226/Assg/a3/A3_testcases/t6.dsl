//import Matrix

// Load matrices
matrix A := input(A3_testcases/t6_A.txt);
matrix B := input(A3_testcases/t6_B.txt);

// Check dimensions
int A_rows := dim(A) / dim(A[0]);
int A_cols := dim(A[0]);
int B_rows := dim(B) / dim(B[0]);
int B_cols := dim(B[0]);

// Perform multiplication if dimensions compatible
if (A_cols == B_rows) {
    matrix C := A * B;
    print(C);
}
else {
    print(9999999);
}