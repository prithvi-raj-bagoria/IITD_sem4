import Matrix

A = input(Demo_input_files/t4_A.txt)
threshold = 1e-3
sum_of_squares = 0.0
for i = 0 to A.rows - 1:
    for j = 0 to A.columns - 1:
        sum_of_squares := sum_of_squares + A[i, j] * A[i, j]
norm = sqrt(sum_of_squares)

while norm > threshold:
    for i = 0 to A.rows - 1:
        for j = 0 to A.columns - 1:
            A[i, j] := A[i, j] * 0.5
    sum_of_squares = 0.0
    for i = 0 to A.rows - 1:
        for j = 0 to A.columns - 1:
            sum_of_squares := sum_of_squares + A[i, j] * A[i, j]
    norm = sqrt(sum_of_squares)

print(A)

// Here t4_A.txt has a 4 x 5 float matrix