#Ansel Smolund, Bryce Butler

def left(e):
    return e[0]
def right(e):
    return e[2]
def op(e):
    return e[1]


def isInside(v,e):
    if type(e) != tuple:
        if v == e:
            return True
        else:
            return False
    else:
        if isInside(v,left(e)):
            return True
        if isInside(v,right(e)):
            return True
        return False 

def solve(v,q):
    if isInside(v,left(q)):
        return solving(v,q)
    elif isInside(v,right(q)):
        r = (right(q),op(q),left(q))
        return solving(v,r)
    else:
        return None
        
def solving(v,q):
    if v == left(q):
        return q
    elif op(left(q)) == '+':
        return solvingAdd(v,q)
    elif op(left(q)) == '-':
        return solvingSubtract(v,q)
    elif op(left(q)) == '*':
        return solvingMultiply(v,q)
    elif op(left(q)) == '/':
        return solvingDivide(v,q)

def solvingAdd(v,q):
    if isInside(v,left(left(q))):
        return (left(left(q)), '=', (right(q), '-',right(left(q))))
    elif isInside(v,right(left(q))):
        return (right(left(q)), '=',(right(q), '-',left(left(q))))
def solvingSubtract(v,q):
    if isInside(v,left(left(q))):
        return (left(left(q)),'=',(right(q), '+',right(left(q))))
    elif isInside(v,right(left(q))):
        return (right(left(q)),'=',(right(left(q)),'-', right(q)))
def solvingMultiply(v,q):
    if isInside(v,left(left(q))):
        return (left(left(q)), '=', (right(q), '/',right(left(q))))
    elif isInside(v,right(left(q))):
        return (right(left(q)), '=',(right(q), '/',left(left(q))))
def solvingDivide(v,q):
    if isInside(v,left(left(q))):
        return (left(left(q)),'=',(right(q), '*',right(left(q))))
    elif isInside(v,right(left(q))):
        return (right(left(q)),'=',(right(left(q)),'/', right(q)))
        
print(isInside('x', 'x'))                          #  True
print(isInside('x', 'y'))                          #  False
print(isInside('x', ('x', '+', 'y')))              #  True
print(isInside('x', ('a', '+', 'b')))              #  False
print(isInside('x', (('m', '*', 'x'), '+', 'b')))  #  True

print(solve('x', (('a', '+', 'x'), '=', 'c')))  #  ('x', '=', ('c', '-', 'a'))
print(solve('x', (('x', '+', 'b'), '=', 'c')))  #  ('x', '=', ('c', '-', 'b'))

print(solve('x', (('a', '-', 'x'), '=', 'c')))  #  ('x', '=', ('a', '-', 'c'))
print(solve('x', (('x', '-', 'b'), '=', 'c')))  #  ('x', '=', ('c', '+', 'b'))

print(solve('x', (('a', '*', 'x'), '=', 'c')))  #  ('x', '=', ('c', '/', 'a'))
print(solve('x', (('x', '*', 'b'), '=', 'c')))  #  ('x', '=', ('c', '/', 'b'))

print(solve('x', (('a', '/', 'x'), '=', 'c')))  #  ('x', '=', ('a', '/', 'c'))
print(solve('x', (('x', '/', 'b'), '=', 'c')))  #  ('x', '=', ('c', '*', 'b'))

print(solve('x', ('y', '=', (('m', '*', 'x'), '+', 'b'))))
# ('x', '=', (('y', '-', 'b'), '/', 'm')




