### Team Members

-   Mohammad Anas: A59020296
-   Nimit Vasavat: A59017505
-   Yatharth Vyas: A59018062

### Installation Instructions:

1. Install Haskell and Cabal
2. `cd chess`
3. `cabal run`

This project is most compatible with cabal v3.10.2.0 and GHC v9.4.7 however it should work with any recent version of cabal and GHC.

### Description

Our CSE 230 project centers around developing a command-line terminal user interface (TUI) for an interactive two-player chess game. The key elements of this game encompass:

![Chess Board](./screenshot.png)

Chess board
Game pieces (both hollow and solid)
Check button
Input box for player movements
Message window to communicate valid/invalid moves, player turns, and win status.

The chess board will aim to incorporate a visualizer to display eligible paths for the selected piece. We will enable text input for piece movements, such as inputting "Bc4" to signify the Bishop moving to the c4 square. If time allows, we will explore implementing piece movement via mouse clicks. Additionally, we plan to highlight opponent pieces in red that fall within the potential path of the selected piece.

This game operates on a turn-based system where each player takes turns inputting their moves on the same device.

To achieve this, we will utilize the brick library to construct a text-based grid representing the chessboard. We will define a Cell type for each square, which can either be empty or contain a piece. A function will be employed to display these cells, and another function will arrange them into a complete board. The application will set up visual attributes and manage events, ultimately displaying the initial state of the chessboard.

### Sources:

-   The feature to highlight possible moves and takedowns was inspired by: https://lichess.org/practice/checkmates/piece-checkmates-i/BJy6fEDf/dW7KIuoY
-   Special Moves: https://www.chess.com/terms/special-chess-moves
-   Color Scheme for Chess board: https://omgchess.blogspot.com/2015/09/chess-board-color-schemes.html
-   Chess ASCII Symbols: https://en.wikipedia.org/wiki/Chess_symbols_in_Unicode
-   Chess Game Title ASCII Art: https://patorjk.com/software/taag/#p=display&f=Graffiti&t=Chess
-   Lenses in Haskell: https://www.fpcomplete.com/haskell/tutorial/lens/
-   For borders in UI: https://github.com/jtdaugherty/brick/blob/master/programs/BorderDemo.hs
-   Tasty documentation (for tests): https://hackage.haskell.org/package/tasty
-   Brick documentation (for UI): https://hackage.haskell.org/package/brick
-   Github Copilot was used to auto-complete some of the code for the project: https://copilot.github.com/
-   ChatGPT was used in error debugging and some code completion: https://chat.openai.com/

## Milestones

### Architecture

Types.hs: This contains the definition for all types that are used in our Project. It includes the different components of chess board such as cells, pieces which are combined into the GameState type.

<li> Cell: This is a type that represents a single cell on the chess board. It can either be empty or contain a piece. It also contains the color of the cell which is used to render the board.
<li> Piece: This is a type that represents a single piece on the chess board. It contains the color of the piece, the type of the piece and the position of the piece on the board.
<li> PieceType: This is a type that represents the type of the piece. It can be King, Queen, Pawn, Rook, Bishop, Knight.
<li> Board: This is a type that represents the chess board. It is a list of lists of cells. It also contains the current turn of the game.
<li> GameState: This is a type that represents the current state of the game. It contains the board, the current turn, the user input which is used to render the board.

Piece.hs: This contains all functions that are related to chess pieces, their movement constraints and getter/setter functions to access them on the chess board.

TestData.hs: This contains some data for initial board and some temporary GHCi style tests to verify the implementation of current functions.

Main.hs: contains the rendering code that is used to draw the chess board and handle the brick app with Gamestate.

ValidateMove.hs: This contains the parser code for reading user input and then translating that to legal moves on the chess board.

#### Flow of the Game

1. You enter a move
2. Check happens to see if it follows convention or not
3. If it does, then executeMove is called
4. executeMove first calls the parseMove to obtain coordinates
5. executeMove then checks if those coordinates are legal using isLegalMove
6. If the move is legal then executeMove calls makeMove
7. makeMove makes the change to the board and returns the updated board
8. executeMove returns the board it receives from makeMove

### Challenges

We had to implement a global variable for the GameState that would be similar to a Monad, with much debugging and some help from TA George, we were able to implement this using the lenses library in Haskell. Another challenging part was to implement the logic for the movement of the pieces. We had to implement a function that would take the current position of the piece and the new position of the piece and then check if the move is legal or not. We had to implement this for all the pieces wherein each piece has a different movement constraint. A issue we faced was to represent the pieces on the chess board. Since the pieces are either black or white, they camouflage with the chessboard and sometimes you cannot distinguish the presence of a black piece on a black chessboard cell which is why we changed the color of the chessboard to a lighter shade of brown.

### Do you expect to meet your goals until the deadline?

We feel that we should be able to make a Chess Game with all rules incorporated before the deadline but it would be difficult to make this a networked game. We are aiming at perfecting what we have before prioritizing more ambitious goals that are good to have such as a "Networked Multiplayer Mode".

### If not, how will you modify your goals?

By first focusing on the more important parts of the project and splitting the work into independent segments, we will work in parallel on finishing the remaining parts for the implementation of a basic single player chess game. We might take up the Networked/Multiplayer mode incase we are left with enough time at the end of the project to build it
