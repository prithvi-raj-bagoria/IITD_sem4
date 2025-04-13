import Matrix

A = input(Demo_input_files/t8_A.txt)   

if Matrix.determinant(A) != 0:
    A_inv = Matrix.inverse(A)
    print(A_inv)
else:
    raise MatrixNotInvertible

// Here t8_A.txt has a 2×2 matrix with determinant 0
