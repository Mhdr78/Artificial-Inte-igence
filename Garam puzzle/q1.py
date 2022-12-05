# x op y = z
def constraint_check(x, y, op, z):
    if x != '_' and y != '_':
        if (len(z) == 2 and z[0] != '_' and z[1] != '_') or (z[0] != '_' and len(z) == 1):
            x = int(x)
            y = int(y)
            z = int(z)
            if op == '+':
                if z != x + y:
                    return False
            elif op == '-':
                if z != x - y:
                    return False
            elif op == '*':
                if z != x * y:
                    return False
        else:
            return True
    else:
        return True


def csp(number, operator, consts):
    consts[0] = constraint_check(number[0], number[1], operator[0], number[2])
    consts[1] = constraint_check(number[3], number[4], operator[1], number[5])
    consts[2] = constraint_check(number[7], number[8], operator[6], number[9])
    consts[3] = constraint_check(number[15], number[16], operator[7], number[17])
    consts[4] = constraint_check(number[18], number[19], operator[8], number[20])
    consts[5] = constraint_check(number[23], number[24], operator[11], number[25])
    consts[6] = constraint_check(number[26], number[27], operator[12], number[28])
    consts[7] = constraint_check(number[30], number[31], operator[17], number[32])
    consts[8] = constraint_check(number[38], number[39], operator[18], number[40])
    consts[9] = constraint_check(number[41], number[42], operator[19], number[43])
    consts[10] = constraint_check(number[16], number[21], operator[9], number[24])
    consts[11] = constraint_check(number[19], number[22], operator[10], number[27])
    consts[12] = constraint_check(number[0], number[6], operator[2], number[11] + number[15])
    consts[13] = constraint_check(number[2], number[7], operator[3], number[12] + number[17])
    consts[14] = constraint_check(number[3], number[9], operator[4], number[13] + number[18])
    consts[15] = constraint_check(number[5], number[10], operator[5], number[14] + number[20])
    consts[16] = constraint_check(number[23], number[29], operator[13], number[34] + number[38])
    consts[17] = constraint_check(number[25], number[30], operator[14], number[35] + number[40])
    consts[18] = constraint_check(number[26], number[32], operator[15], number[36] + number[41])
    consts[19] = constraint_check(number[28], number[33], operator[16], number[37] + number[43])
    for i in range(len(consts)):
        if consts[i] is False:
            return False
    return True


def isAnswer(number):
    for i in range(len(number)):
        if number[i] == '_':
            return False
    return True


def print_puzzle(numbers):
    print numbers
    f = open("output.out", "a")
    num_counter = 0
    op_counter = 0
    for row in range(9):
        if row == 0:
            str = (numbers[num_counter] + operators[op_counter] + numbers[num_counter + 1] + "=" + numbers[
                   num_counter + 2] + "\t" + numbers[num_counter + 3] + operators[op_counter + 1] + numbers[
                   num_counter + 4] + "=" + numbers[num_counter + 5])
            f.write(str)
            f.write("\n")
            print(str)

            num_counter += 6
            op_counter += 2
            str = (operators[op_counter] + "\t" + operators[op_counter + 1] + "\t" + operators[op_counter + 2] + "\t" +
                   operators[op_counter + 3])
            f.write(str)
            f.write("\n")
            print(str)
            op_counter += 4

        elif row == 1:
            str = (numbers[num_counter] + "\t" + numbers[num_counter + 1] + operators[op_counter] + numbers[
                   num_counter + 2] + "=" + numbers[num_counter + 3] + "\t" + numbers[num_counter + 4])
            f.write(str)
            f.write("\n")
            print(str)
            num_counter += 5
            op_counter += 1
            str = "=\t=\t=\t="
            f.write(str)
            f.write("\n")
            print(str)
        elif row == 2:
            str = (numbers[num_counter] + "\t" + numbers[num_counter + 1] + "\t" + numbers[num_counter + 2] + "\t" +
                  numbers[num_counter + 3])
            f.write(str)
            f.write("\n")
            print(str)
            num_counter += 4
        elif row == 3:
            str = (numbers[num_counter] + operators[op_counter] + numbers[num_counter + 1] + "=" + numbers[
                   num_counter + 2] + "\t" + numbers[num_counter + 3] + operators[op_counter + 1] + numbers[
                   num_counter + 4] + "=" + numbers[num_counter + 5])
            f.write(str)
            f.write("\n")
            print(str)
            num_counter += 6
            op_counter += 2
            str = "  " + operators[op_counter] + "\t\t\t" + operators[op_counter + 1]
            f.write(str)
            f.write("\n")
            print(str)
            op_counter += 2
        elif row == 4:
            str = "  " + numbers[num_counter] + "\t\t\t" + numbers[num_counter + 1]
            f.write(str)
            f.write("\n")
            print(str)
            num_counter += 2
            str = "  = \t    ="
            print(str)
        elif row == 5:
            str = (numbers[num_counter] + operators[op_counter] + numbers[num_counter + 1] + "=" + numbers[
                   num_counter + 2] + "\t" + numbers[num_counter + 3] + operators[op_counter + 1] + numbers[
                   num_counter + 4] + "=" + numbers[num_counter + 5])
            f.write(str)
            f.write("\n")
            print(str)
            num_counter += 6
            op_counter += 2
            str = (operators[op_counter] + "\t" + operators[op_counter + 1] + "\t" + operators[op_counter + 2] + "\t" +
                   operators[op_counter + 3])
            f.write(str)
            f.write("\n")
            print(str)
            op_counter += 4
        elif row == 6:
            str = (numbers[num_counter] + "\t" + numbers[num_counter + 1] + operators[op_counter] + numbers[
                   num_counter + 2] + "=" + numbers[num_counter + 3] + "\t" + numbers[num_counter + 4])
            print(str)
            f.write(str)
            f.write("\n")
            num_counter += 5
            op_counter += 1
            str = "=\t=\t=\t="
            f.write(str)
            f.write("\n")
            print (str)
        elif row == 7:
            str = (numbers[num_counter] + "\t" + numbers[num_counter + 1] + "\t" + numbers[num_counter + 2] + "\t" +
                  numbers[num_counter + 3])
            f.write(str)
            f.write("\n")
            print(str)
            num_counter += 4
        elif row == 8:
            str = (numbers[num_counter] + operators[op_counter] + numbers[num_counter + 1] + "=" + numbers[
                   num_counter + 2] + "\t" + numbers[num_counter + 3] + operators[op_counter + 1] + numbers[
                   num_counter + 4] + "=" + numbers[num_counter + 5])
            f.write(str)
            f.write("\n")
            print(str + "\n")



def garam_solver(numbers, domains, operators, index, check):
    if not csp(numbers, operators, check):
        return False
    if isAnswer(numbers):
        f = open("output.out", "a")
        print("Garam Puzzle solution:")
        print_puzzle(numbers)
        f.write("\n")
        return True
    else:
        for x in range(len(numbers)):
            if numbers[x] == '_':
                index = x
                break
        possibilities = domains[index]
        for z in range(len(possibilities)):
            numbers[index] = possibilities[z]
            if garam_solver(numbers, domains, operators, index, check):
                return
        numbers[index] = '_'


def readFile(FileName, lines):
    file = open(FileName, 'r')
    lines = file.read().splitlines()
    return lines


if __name__ == '__main__':
    lines = []
    lines = readFile("input.in", lines)
    for garam in range(0, len(lines), 2):
        operators = [lines[garam][i:i + 1] for i in range(0, len(lines[garam]), 1)]
        numbers = lines[garam + 1].split(',')
        domains = [['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']] * len(numbers)
        consts = [True] * len(operators)
        count = 0
        index = 0
        for i in range(len(numbers)):
            if numbers[i] != '_':
                domains[i] = []

        garam_solver(numbers, domains, operators, index, consts)
