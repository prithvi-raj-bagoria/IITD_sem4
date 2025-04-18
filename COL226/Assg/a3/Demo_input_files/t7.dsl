// Add matrix and vector - using broadcast pattern

matrix A := input(Demo_input_files/t7_A.txt);
vector v := input(Demo_input_files/t7_v.txt);

// Create a result matrix
matrix C := A;

// Add vector to each row of the matrix
for (int i := 0; i < dim(A)[0]; i := i + 1) {
    for (int j := 0; j < dim(A)[1]; j := j + 1) {
        // Assuming v has the same length as matrix columns
        // Add vector element to corresponding matrix element
        C[i][j] := A[i][j] + v[j];
    }
}

print(C);