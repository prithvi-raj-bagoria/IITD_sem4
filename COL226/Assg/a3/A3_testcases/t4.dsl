//import Matrix

// Matrix normalization algorithm

// Load matrix
matrix A := input(A3_testcases/t4_A.txt);
float threshold := 1e-3;
float sum_of_squares := 0.0;

// Calculate initial Frobenius norm
int rows := dim(A) / dim(A[0]);
int cols := dim(A[0]);

// Calculate initial sum of squares
for (int i := 0; i < rows; i := i + 1) {
    for (int j := 0; j < cols; j := j + 1) {
        sum_of_squares := sum_of_squares + A[i][j] * A[i][j];
    }
}
float norm := sqrt(sum_of_squares);

// Scale matrix until norm is below threshold
while (norm > threshold) {
    // Scale matrix by 0.5
    for (int i := 0; i < rows; i := i + 1) {
        for (int j := 0; j < cols; j := j + 1) {
            A[i][j] := A[i][j] * 0.5;
        }
    }
    
    // Recalculate norm
    sum_of_squares := 0.0;
    for (int i := 0; i < rows; i := i + 1) {
        for (int j := 0; j < cols; j := j + 1) {
            sum_of_squares := sum_of_squares + A[i][j] * A[i][j];
        }
    }
    norm := sqrt(sum_of_squares);
}

print(A);