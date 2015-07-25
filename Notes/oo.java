import java.util.*;

// Java
/*

  Object oriented concepts:
   - subtyping
   - inheritance
   - dynamic dispatch
  
  Key focus: interface vs. implementation
   why is it a good thing to separate interface/impl?

   o flexibility for the implementer
     - can change implementation without affecting clients
       as long as the interface doesn't change
     - can monetize your implementation
     - can hide private details
       - my SSN
       - my proprietary algorithm
   o client doesn't need to care about implementation details
     
*/

// Interfaces (types)

// type for a set of strings
interface Set {
    boolean contains(String s);
    void addElem(String s);
    int size();
}

/* Separating interface from implementation simplifies
   reasoning about correctness.
   Splits reasoning into two pieces:

   1. Check that an implementation actually obeys the interface
      - supports all the operations of the interface
      - implementer doesn't know or care about the clients

   2. Check that clients use the interface properly
      - invoke operations with arguments of the right type
      - client doesn't know or care about the implementations
   
*/

/* Declares a new type Set

   Declares the operations supported by a Set

   Can only specify methods
*/

class Client {
    void myClient(Set s) {
	// if(s.contains("hi"))
	    s.addElem("bye");
	System.out.println("size is " + s.size());
    }
}

// an implementation of a set of strings
class Set1 implements Set {
    private List<String> l = new LinkedList<String>();

    public boolean contains(String s) {
	return l.contains(s);
    }

    public void addElem(String s) {
	if (!(this.contains(s)))
	    l.add(s);
    }

    public int size() {
	return l.size();
    }
}

class Main {
    public static void main(String[] args) {
	Client c = new Client();
	c.myClient(new Set2(0));
    }
}

class Set2 implements Set {

    private String[] elems;
    private int size;

    Set2(int initialSize) {
	if (initialSize <= 0)
	    initialSize = 10;
	this.elems = new String[initialSize];
	this.size = 0;
    }

    public boolean contains(String s) {
	for (int i = 0; i < this.size; i++) {
	    if (s.equals(elems[i]))
		return true;
	}
	return false;
    }

    public void addElem(String s) {
	if(this.contains(s))
	    return;
	
	if (size == elems.length) {
		// ugly resizing
	    String[] newarr = new String[size * 2];
	    for (int i = 0; i < size; i++)
		newarr[i] = elems[i];
	    this.elems = newarr;
	}
	elems[size] = s;
	size++;
    }

    public int size() { return this.size; }
}

/* Kinds of polymorphism in Java:

   1. Parametric polymorphism (aka Generics)
      - can declare types that are parameterize by type variables

   2. Subtype polymorphism
      - types have a relation among them (the subtyping relation)
      - if S is a subtype of T, then can use an object of type S
        wherever an object of type T is expected

*/

class Generics {
    public static void main(String[] args) {
	
	List<String> l1 = new ArrayList<String>();
	List<Integer> l2 = new ArrayList<Integer>();

	l1.add("hello");
	l2.add(34); // implicit conversion from int to Integer

	    // l2.add("hello"); // static type error
    }
}

// a generic Set
interface GenericSet<E> {
    boolean contains(E s);
    void addElem(E s);
    int size();
}

class GenericSet1<T> implements GenericSet<T> {
    private List<T> l = new LinkedList<T>();

    public boolean contains(T s) {
	return l.contains(s);
    }

    public void addElem(T s) {
	if (!(this.contains(s)))
	    l.add(s);
    }

    public int size() {
	return l.size();
    }
}

// Subtyping

// a subtype of Set
interface RemovableSet extends Set {
    void remove(String s);
}

class NewClient {

    void m(RemovableSet rs) {
	Client c = new Client();
	c.myClient(rs);
	Set s = rs;
	rs.remove("hello");
	    // s.remove("hello"); // type error
	this.m(rs);
	    // this.m(s); // type error
    }

    Set union(Set s1, Set s2) {
	    // ...
	return s1; // not a real implementation
    }
    
}
