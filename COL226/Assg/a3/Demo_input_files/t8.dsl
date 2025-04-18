// Calculate matrix inverse if possible

matrix A := input(Demo_input_files/t8_A.txt);

float det_val := det(A);
if (det_val != 0.0) {
    matrix A_inv := inverse(A);
    print(A_inv);
}
else {
    // Using print with string not supported, use numeric error code
    print(9999999);  // Error code for non-invertible matrix
}