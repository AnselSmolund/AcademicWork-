import types
def main():
	x = 7+(4/2)
	y = 10+2
	
	print evaluate(((x),'+',(y)))
		
		

def left(e):
	return e[0]
def right(e):
	return e[2]
def op(e):
	return e[1]
def evaluate(e):
	if type(e) == types.TupleType:
		L = left(e)
		O = op(e)
		R = right(e)
		if O == "+":
			return evaluate(L) + evaluate(R)
		elif O == "-":
			return evaluate(L) - evaluate(R)
		elif O == "/":
			return evaluate(L)/evaluate(R)
		elif 0 == "*":
			return evaluate(L)*evaluate(R)
		else:
			return None
	else: 
		return e 
 
  
 
    


if __name__ == '__main__':
   main()
   
   
