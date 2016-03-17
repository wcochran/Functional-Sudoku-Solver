/*
 * Sudoku Solver for JavaScript using Backtracking.
 * Author: W. Cochran wcochran@wsu.edu
 *
 * Purely functional solution:
 *    * Immutable SudokoGrid (copying used instead of mutation)
 *    * No iteration (tail recursion used).
 *    * No sideffects (all function simple return value).
 */

//
// SudokuGrid contructor.
// Creates a SudokuGrid instance from a string, array, or another SudokuGrid.
// Internally the 9x9 grid is represented with an array of 81 integers
// in row-major order. 0 represents an empty cell, where 1..9 represents an
// entered value.
//
function SudokuGrid(obj) {
    if (typeof(obj) === 'string')
        this.grid =  Array.prototype.map.call(obj, function (c) {
            if (c == ".")
                return 0
            return c.charCodeAt(0) - "0".charCodeAt(0)
        });
    else if (Array.isArray(obj))
        this.grid = obj.slice(0);
    else if (obj instanceof SudokuGrid)
        this.grid = obj.grid.slice(0);
    return this;
}

//
// Fetch number from grid at (row, col).
//
SudokuGrid.prototype.elem = function(row, col) {
    return this.grid[row*9 + col]
}

//
// Returns new grid with updated value 'num' stored at (row, col).
//
SudokuGrid.prototype.update = function(row, col, num) {
    var puzzle = new SudokuGrid(this);
    puzzle.grid[row*9 + col] = num;
    return puzzle;
}

//
// Return true iff there is value equal to 'num' stored in the given 'row.'
//
SudokuGrid.prototype.conflictingNumberInRow = function(num, row) {
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
SudokuGrid.prototype.conflictingNumberInColumn = function(num, col) {
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
SudokuGrid.prototype.conflictingNumberInBlock = function(num, row, col) {
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
SudokuGrid.prototype.conflictingNumber = function(num, row, col) {
    return this.conflictingNumberInRow(num, row) ||
           this.conflictingNumberInColumn(num, col) ||
           this.conflictingNumberInBlock(num, Math.trunc(row/3)*3, Math.trunc(col/3)*3);
}

//
// Returns the index (between 0 and 81 inclusive)
// of the first zero in the linear grid.
// Returns -1 if there are no zeroes in the grid.
//
SudokuGrid.prototype.unassignedLocation = function() {
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
SudokuGrid.prototype.solve = function() {
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
SudokuGrid.prototype.solveStartingAtCell = function(row, col) {
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
var puzzle = new SudokuGrid(puzzleStr);
var soln = puzzle.solve();
process.stdout.write(soln.grid.toString());

