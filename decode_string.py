import re

first_multiple_input = input().rstrip().split()
n = int(first_multiple_input[0])
m = int(first_multiple_input[1])
matrix = []
for _ in range(n):
    matrix_item = input()
    matrix.append(matrix_item)

# Sample matrix
# n = 8
# m = 3
# matrix = ['7 3', 'Tsi', 'h%x', 'i #', 'sM ', '$a ', '#t%', 'ir!']
s = ""
for i in range(m):
    for j in range(n):
        s += matrix[j][i]

replaced = re.sub(r'\s|[0-9]', '', s)
replaced = re.sub(r'[!@$#%& ]+', r' ', replaced).strip()
replaced += s[re.search(r'[a-zA-Z][!@$#%&\s]*$', s).start() + 1:]
replaced = re.sub(r'\s+', ' ', replaced)

print(replaced)
