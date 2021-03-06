/*
 * Sudoku Solver (version 2) for JavaScript using Backtracking.a
 * Author: W. Cochran wcochran@wsu.edu
 *
 * Instead of using new, we use Object.create() and do things the
 * way a protypical language should.
 *
 * Purely functional solution:
 *    * Immutable SudokoGrid (copying used instead of mutation)
 *    * No iteration (tail recursion used).
 *    * No sideffects (all function simple return value).
 */

//
// See the following Stack Overflow question for a discussion
// about using Object.create() versus "new":
// http://goo.gl/pFco47
//

//
// Base object from which all puzzle instances inherit from.
// The 9x9 Sudoku grid is encoded as a grid of 81 integers
// in row-major order with values between 0 and 9 where 
// 0 means "empty" cell.
//
var sudokuPuzzle = {
    grid : Array(81).fill(0)
};

//
// Create a new puzzle by copying the given array.
//
sudokuPuzzle.createFromArray = function(array) {
    var puzzle = Object.create(sudokuPuzzle);
    puzzle.grid = array.slice(0);
    return puzzle;
}

//
// Create puzzle from string of 81 characters ('.' is for
// empty cell, '1'..'9' for numbers.
//
sudokuPuzzle.createFromString = function(string) {
    var puzzle = Object.create(sudokuPuzzle);
    puzzle.grid = Array.prototype.map.call(string, function (c) {
        if (c == ".")
            return 0
        return c.charCodeAt(0) - "0".charCodeAt(0)
    });
    return puzzle;
}

//
// Clone a puzzle.
// Note is is importand to inherit from base object (to
// avoid long inheritance chains).
// http://goo.gl/pFco47
//
sudokuPuzzle.clone = function() {
    var puzzle = Object.create(sudokuPuzzle);
    puzzle.grid = this.grid.slice(0);
    return puzzle;
}

//
// Fetch number from grid at (row, col).
//
sudokuPuzzle.elem = function(row, col) {
    return this.grid[row*9 + col]
}

//
// Returns new grid with updated value 'num' stored at (row, col).
//
sudokuPuzzle.update = function(row, col, num) {
    var puzzle = this.clone();
    puzzle.grid[row*9 + col] = num;
    return puzzle;
}

//
// Return true iff there is value equal to 'num' stored in the given 'row.'
//
sudokuPuzzle.conflictingNumberInRow = function(num, row) {
    var me = this;
    function conflicting(col) {
        if (col >= 9)
            return false;
        else if (me.elem(row, col) == num)
            return true;
        else
            return conflicting(col+1);
    }
    return conflicting(0);
}

//
// Return true iff there is value equal to 'num' stored in the given 'column.'
//
sudokuPuzzle.conflictingNumberInColumn = function(num, col) {
    var me = this;
    function conflicting(row) {
        if (row >= 9)
            return false;
        else if (me.elem(row, col) == num)
            return true;
        else
            return conflicting(row+1);
    }
    return conflicting(0);
}

//
// Return true iff there is value equal to 'num' stored in the 3x3 block
// with corner at (row, col).
//
sudokuPuzzle.conflictingNumberInBlock = function(num, row, col) {
    var me = this;
    function conflicting(r, c) {
        if (r >= 3)
            return false;
        else if (me.elem(row + r, col + c) == num)
            return true;
            else if (c < 2)
                return conflicting(r, c+1);
        else
            return conflicting(r+1, 0);
    }
    return conflicting(0,0);
}


//
// Returns tree iff there is a value equal to num in the same row, 
// column, or 3x3 block indexed by (row, col).
//
sudokuPuzzle.conflictingNumber = function(num, row, col) {
    return this.conflictingNumberInRow(num, row) ||
           this.conflictingNumberInColumn(num, col) ||
           this.conflictingNumberInBlock(num, Math.trunc(row/3)*3, Math.trunc(col/3)*3);
}

//
// Returns the index (between 0 and 81 inclusive)
// of the first zero in the linear grid.
// Returns -1 if there are no zeroes in the grid.
//
sudokuPuzzle.unassignedLocation = function() {
    var me = this;
    function zeroIndex(index) {
        if (index >= 81)
            return -1;
        else if (me.grid[index] == 0)
            return index;
        else
            return zeroIndex(index + 1)
    }
    return zeroIndex(0);
}

//
// Top-level solved function.
// Returns solved Sudoku grid (or null if no solution).
//
sudokuPuzzle.solve = function() {
    var index = this.unassignedLocation();
    if (index < 0)
        return this; // solved;
    else {
        var row = Math.trunc(index / 9);
        var col = index % 9;
        var soln = this.solveStartingAtCell(row, col);
        if (soln)
            return soln;
        else
            return null;
    }
}

//
// Attempts to solve grid using all digits 1..9 at (row,col).
//
sudokuPuzzle.solveStartingAtCell = function(row, col) {
    var me = this;
    function solveWithNumber(num) {
        if (num > 9)
            return null;
        else if (me.conflictingNumber(num, row, col))
            return solveWithNumber(num + 1);
        else {
            var soln = me.update(row, col, num).solve();
            if (soln)
                return soln;
            else
                return solveWithNumber(num + 1);
        }
    }
    return solveWithNumber(1);
}

var puzzleStr = ".94...13..............76..2.8..1.....32.........2...6.....5.4.......8..7..63.4..8";
var puzzle = sudokuPuzzle.createFromString(puzzleStr);
var soln = puzzle.solve();
process.stdout.write(soln.grid.toString());

