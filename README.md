# Chess Game

Our CSE 230 project centers around developing a command-line terminal user interface (TUI) for an interactive two-player chess game. The key elements of this game encompass:

Chess board
Game pieces (both hollow and solid)
Check button
Input box for player movements
Message window to communicate valid/invalid moves, player turns, and win status.

The chess board will aim to incorporate a visualizer to display eligible paths for the selected piece. We will enable text input for piece movements, such as inputting "Bc4" to signify the Bishop moving to the c4 square. If time allows, we will explore implementing piece movement via mouse clicks. Additionally, we plan to highlight opponent pieces in red that fall within the potential path of the selected piece.

This game operates on a turn-based system where each player takes turns inputting their moves on the same device.

To achieve this, we will utilize the brick library to construct a text-based grid representing the chessboard. We will define a Cell type for each square, which can either be empty or contain a piece. A function will be employed to display these cells, and another function will arrange them into a complete board. The application will set up visual attributes and manage events, ultimately displaying the initial state of the chessboard.

