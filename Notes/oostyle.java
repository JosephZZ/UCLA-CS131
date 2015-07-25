
/* OO Style

   objects that talk to each other by sending messages

   each object knows how to do certain things
   - publishes what it knows how to do through an interface

   clients interact with the object only through the interface

   should never have to ask an object who they are / how they are implemented
   
   strong separation of interface and implementation
   
*/

// simple example: a chess game
/*

interface Piece {
    boolean isLegalMove(int x, int y);
}

abstract class PieceImpl implements Piece {
    int x, y;
	// ...
}

class Rook extends PieceImpl implements Piece {
	// ...
    boolean isLegalMove(int x, int y) { ... }
}

class Pawn extends PieceImpl implements Piece {
	// ...
    boolean isLegalMove(int x, int y) { ... }
}

class Board {
    void move(Piece p, int x, int y) {
	if (p.isLegalMove(x,y)) {
		// ... make the move
	}
    }
}
*/
