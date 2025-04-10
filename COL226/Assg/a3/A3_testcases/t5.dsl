//import Matrix

// Matrix addition with dimension checking

// Load matrices
matrix A := input(A3_testcases/t5_A.txt);
matrix B := input(A3_testcases/t5_B.txt);

// Check dimensions
int A_rows := dim(A) / dim(A[0]);
int A_cols := dim(A[0]);
int B_rows := dim(B) / dim(B[0]);
int B_cols := dim(B[0]);

// Perform addition if dimensions match
if (A_rows == B_rows && A_cols == B_cols) {
    matrix C := A + B;
    print(C);
}
else {
    print(9999999);
}