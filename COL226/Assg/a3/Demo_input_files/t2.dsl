import Matrix

A = input(t2_A.txt)
B = input(t2_B.txt)
D = input(t2_D.txt)
u = input(t2_u.txt)

C = Matrix.add(A, B)

E = Matrix.multiply(C, D)

if Matrix.determinant(E) != 0:
    E_inverse = Matrix.inverse(E)
    x = Matrix.vector_multiply(E_inverse, u)
    print(x)
else:
    raise MatrixNotInvertible

// Here t2_A.txt, t2_B.txt have 4 x 5 float matrix, t2_D.txt has 5 x 4 float matrix, and t2_u.txt has a 4 x 1 float vector