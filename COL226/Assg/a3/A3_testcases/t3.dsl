//import Matrix

// Vector sum calculation

// Load vector
vector v := input(A3_testcases/t3_v.txt);
float sum_result := 0.0;

// Sum all elements in the vector
for (int i := 0; i < dim(v); i := i + 1) {
    sum_result := sum_result + v[i];
}

// Scale the result
float ans := 2.5 * sum_result;
print(ans);

// Here t3_v.txt has a 4 x 1 float vector