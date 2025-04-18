// Calculate sum of vector elements

vector v := input(Demo_input_files/t3_v.txt);
float sum_result := 0.0;

for (int i := 0; i < dim(v); i := i + 1) {
    sum_result := sum_result + v[i];
}

float ans := 2.5 * sum_result;
print(ans);