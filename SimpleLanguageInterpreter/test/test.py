
array = {}
#array[0] = {}
#array[1] = {}

n = 3

array[0] = ["0", "true", "false"]

for it in range(1, n):
    array[it] = []
    for key in array[it - 1]:
        array[it].append("succ " + key)
        array[it].append("pred " + key)
        array[it].append("iszero " + key)

        for key1 in array[it - 1]:
            for key2 in array[it - 1]:
                array[it].append("if " + key + " then " + key1 + " else " + key2)

for it in range (0, n):
    for testcase in array[it]:
        print (testcase)

