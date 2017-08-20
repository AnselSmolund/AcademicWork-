//Ansel Smolund

class HashedBST<Key,Value>
{

  private class TreeNode
  {
    private int   hashVal;    
    private ValueNode value;  
    private TreeNode  left;   
    private TreeNode  right;  


    private TreeNode(int hashVal, ValueNode value)
    {
      this.hashVal   = hashVal;
      this.value = value;
      this.left  = null;
      this.right = null;
    }
  }
  private class ValueNode{
      private Key key;
      private Value value;
      private ValueNode next;
      private ValueNode(Key key, Value value, ValueNode next){
          this.key = key;
          this.value = value;
          this.next = next;
      }
  }
  
   
  private TreeNode root; 
  public HashedBST()
  {
   
    root = null;
  }
  
  public Value get(Key key)
  {
    TreeNode subtree = root;
    while (subtree != null)
    {
      int test = hash(key);
      if (test < subtree.hashVal)
      {
        subtree = subtree.left;
      }
      else if (test > subtree.hashVal)
      {
        subtree = subtree.right;
      }
      else
      {
        ValueNode temp = subtree.value;
        while(temp!=null){
            if(key.equals(temp.key)){
                return temp.value;
            }
            else{
                temp = temp.next;
            }
        }
        
      }
    }
    throw new IllegalStateException("No such key.");
  }

  public int height()
  {
    return height(root);
  }

  private int height(TreeNode subtree)
  {
    if (subtree == null)
    {
      return 0;
    }
    else
    {
      int left  = height(subtree.left);
      int right = height(subtree.right);
      if (left > right)
      {
        return left + 1;
      }
      else
      {
        return right + 1;
      }
    }
  }

  private int hash(Key key){
      if(key == null){
          return 0;
      }
      return key.hashCode();
  }
  public boolean isEmpty()
  {
    return root == null;
  }


  public void put(Key key, Value value)
  {
    int test = hash(key);  
    if (root == null)
    {
      root = new TreeNode(test, new ValueNode(key,value,null));
      
    }
    else
    {
        
      TreeNode subtree = root;
      while (true)
      {
        
        if (test < subtree.hashVal)
        {
          if (subtree.left == null)
          {
            
            subtree.left = new TreeNode(test, new ValueNode(key,value,null));
            
          
            return;
          }
          else
          {
            subtree = subtree.left;
          }
        }
        else if (test > subtree.hashVal)
        {
          if (subtree.right == null)
          {
            
          
            subtree.right = new TreeNode(test, new ValueNode(key,value,null));
            return;
          }
          else
          {
            subtree = subtree.right;
          }
        }
        else
        {
          ValueNode Vtemp = subtree.value;
          while(Vtemp!=null){
              if(key.equals(Vtemp.key)){
                  Vtemp.value = value; 
                  return;
              }
              else{
                  Vtemp = Vtemp.next;
              }
          }
          subtree.value = new ValueNode(key,value,subtree.value);
          return;
        }
      }
    }
  }

}

class ProjectThreeDriver  
{  
  private final static String[] reserved =  
   { "abstract",     "assert",    "boolean",     "break",  
     "byte",         "case",      "catch",       "char",  
     "class",        "const",     "continue",    "default",  
     "do",           "double",    "else",        "extends",  
     "final",        "finally",   "float",       "for",  
     "goto",         "if",        "implements",  "import",  
     "instanceof",   "int",       "interface",   "long",  
     "native",       "new",       "package",     "private",  
     "protected",    "public",    "return",      "short",  
     "static",       "super",     "switch",      "synchronized",  
     "this",         "throw",     "throws",      "transient",  
     "try",          "void",      "volatile",    "while" };  
  
  public static void main(String [] args)  
  {  
    HashedBST<String, Integer> hbst = new HashedBST<String, Integer>();  
  
    for (int index = 0; index < reserved.length; index += 1)  
    {  
      hbst.put(reserved[index], index);  
      
    }  
    
   
    System.out.println(hbst.height());  
  
    for (int index = 0; index < reserved.length; index += 1)  
    {  
      System.out.format("%02d %s", hbst.get(reserved[index]), reserved[index]);  
      System.out.println();  
    }  
  }  
}
/* 
16
00 abstract
01 assert
02 boolean
03 break
04 byte
05 case
06 catch
07 char
08 class
09 const
10 continue
11 default
12 do
13 double
14 else
15 extends
16 final
17 finally
18 float
19 for
20 goto
21 if
22 implements
23 import
24 instanceof
25 int
26 interface
27 long
28 native
29 new
30 package
31 private
32 protected
33 public
34 return
35 short
36 static
37 super
38 switch
39 synchronized
40 this
41 throw
42 throws
43 transient
44 try
45 void
46 volatile
47 while
*/
