#CS 463 Homework 1, Python Code
#Hailey Lee, G01254390

#Given a positive integer n, 
#return a list of its prime factors, in increasing order.
def prime_factors(n):
    factors = []
    prime = False

    if n == 1:
        return []
    elif n == 2 or n == 3 or n == 5 or n == 7:
        factors.append(n) #simple, one-digit prime numbers
    else:
        while n != 1:
            for i in range(2, n + 1):
                if n % i == 0:
                    for j in range(2, i + 1):
                        if j == i: #when got to the end
                            prime = True
                            break
                        if i % j == 0: 
                            prime = False
                            break 
                if prime == True:
                    factors.append(i)
                    n //= i
                    prime = False
                    break

    return factors


#Given two positive integers a and b, 
#return whether they are co-prime or not.
def coprime(a,b):
    for i in range(2, min(a, b) + 1, 1):
        if a % i == 0 and b % i == 0:
                return False

    return True


#Given a non-negative integer n,
#calculate the nth tribonnaci number. 
def trib(n):
    if n == 0 or n == 1 or n == 2: #base cases
        return 1
    
    x,y,z=1,1,1
    sum = 3

    if n != 3:
        while n - 3 > 0: #number of updates needed
            x = y
            y = z
            z = sum
            sum = x + y + z
            n -= 1

    return sum


#Given a list of integers xs,
#find the two largest values and return them in a list (largest first).
def max_two(xs):
    if len(xs) == 0 or len(xs) == 1:
        return xs

    #temp = xs.copy() copying the given list
    temp = []
    for x in xs:
       temp.append(x)

    maxs = []
    index = 0

    for i in range(0, len(temp)): #find the first max
        if temp[i] >= temp[index]:
            index = i

    maxs.append(temp[index])
    temp.pop(index)     #remove the first max
    index = 0           #reset the index

    for i in range(0, len(temp)):
        if  temp[i] >= temp[index]:
            index = i

    maxs.append(temp[index])
    return maxs

#Given a list of values, 
#create the list whose values are in the opposite order. 
def reversed(xs):
    reverse = []

    for i in xs:
        reverse.insert(0, i)

    return reverse


#Given a list of lists, when it is rectangular (all rows have same length), 
#create and return the list of lists that contains the same values, but rotated clockwise. 
#If this 2D list is not rectangular, raise a ValueError with the message "not rectangular". 
def clockwise(grid):
    if len(grid) == 0:
        return grid

    cw = []
    r = len(grid)       #number of rows
    c = len(grid[0])    #number of columns (based on the first list)

    for i in range(0, len(grid)):
        if len(grid[i]) != c: #when the number of columns are different (aka not rectangle)
            raise ValueError("not rectangular")

    for i in range(0, c):
        temp = []
        for j in range(0, r):
            temp.insert(0, grid[j][i])
        cw.append(temp)

    return cw


#Given a list of bool values, 
#return True if any of them is True, 
#and return False if every single one of them is False. 
def any(xs):
    for bool in xs:
        if bool == True:
            return True

    return False


#Given a predicate function p (which accepts a single argument and returns a boolean), 
#as well as a list of values xs, create a list of all items from xs that pass predicate function p. 
def select(p, xs):
    select = []

    for i in xs:
        if p(i) == True:
            select.append(i)

    return select


#Given a two-argument function and two lists of arguments to supply, 
#create a list of the results of applying the function to each same-indexed pair of arguments from the two lists. 
def zip_with(f, xs, ys):
    zip = []
    ziplen = min(len(xs), len(ys))

    for i in range(0, ziplen, 1):
        zip.append(f(xs[i], ys[i]))

    return zip


#Given positive ints r and c indicating number of rows and columns, 
#create a 2D list that represents the "augmented identity matrix" with that dimension: 
#It's the k x k identity matrix (where k = min(r,c)), 
#and augmented rightwards or downwards as needed with zeroes in order to be of size r x c. 
def augdentity(r, c):
    aug = []
    k = 0
    
    for i in range(0, r):
        temp = []
        for j in range(0, c):
            if k == i and k == j:
                temp.append(1)
            else:
                temp.append(0)
        aug.append(temp)
        k += 1      #k will keep updated (k will be checked inside the for loop)

    return aug
