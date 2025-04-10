//import Matrix

// Matrix-vector addition (broadcasting)

// Load matrix and vector
matrix A := input(A3_testcases/t7_A.txt);
vector v := input(A3_testcases/t7_v.txt);

// Check dimensions
int A_rows := dim(A) / dim(A[0]);
int A_cols := dim(A[0]);
int v_size := dim(v);

// Broadcast vector to each row of matrix if compatible
if (v_size == A_cols) {
    matrix C := A;
    for (int i := 0; i < A_rows; i := i + 1) {
        for (int j := 0; j < A_cols; j := j + 1) {
            C[i][j] := A[i][j] + v[j];
        }
    }
    print(C);
}
else {
    print(9999999);
}